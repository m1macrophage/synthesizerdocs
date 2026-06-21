# license:BSD-3-Clause-Clear
# copyright-holders:m1macrophage

# Proof-of-concept implementation of a band-limitted oscillator, including
# support for sync.
#
# Intended for quick experimentation and verification of the relevant algorithms.
# Not well-documented, nor efficient. The production variant of this is:
# https://github.com/mamedev/mame/blob/master/src/devices/sound/va_vco.cpp

DEBUG_LEVEL <- 0
LOG_DISC <- 1

debuglog <- function(level, ...)
{
	if (level <= DEBUG_LEVEL) cat(...)
}

# Based on
# https:#www.martin-finke.de/articles/audio-plugins-018-polyblep-oscillator
# But removed the baked-in scale factor of 2, and made it configurable.
poly_blep <- function(phase, step, height)
{
	# if (!((phase >= 1 - step && phase < 1) || (phase >= 0 && phase <= step))) return (0)
	stopifnot((phase >= 1 - step && phase < 1) || (phase >= 0 && phase <= step))
	t <- phase / step
	pb1 <- height * (t - 0.5 * t * t - 0.5)
	t <- (phase - 1) / step
	pb2 <- height * (t + 0.5 * t * t + 0.5)
	return (ifelse(phase < step, pb1, ifelse(phase > 1 - step, pb2, 0)))
}

# Returns the correction to be applied to the current and next sample (vector of size 2)
# Unused for now.
poly_blep_corrections <- function(disc, step)
{
	corr <- vapply(disc, FUN.VALUE=numeric(2), FUN=function(d)
	{
		curr_corr <- d[2] * poly_blep(d[1], step, 1)
		next_corr <- d[2] * poly_blep((d[1] + step) %% 1, step, 1)
		return (c(curr_corr, next_corr))
	})
	return (rowSums(corr))
}

# Implementation is based on:
# https:#dsp.stackexchange.com/questions/54790/polyblamp-anti-aliasing-in-c
poly_blamp <- function(phase, step)
{
	# For now, only using for a single sample correction before and after the discontinuity.
	TOLERANCE <- 1e-16
	stopifnot(phase >= 0 && phase <= step + TOLERANCE)

	y <- 0
	if (0 <= phase && phase < 2 * step)
	{
		x <- phase / step
		u = 2 - x
		u2 = u * u
		y <- y - u * u2 * u2

		if (phase < step)
		{
			v <- 1 - x
			v2 <- v * v
			y <- y + 4 * v * v2 * v2
		}
	}
	return (y * step / 15)

}

check_disc <- function(osc_phase, step, sync_info, label, sample, disc_phase_fn, ...)
{
	# Is there a discontinuity before the next sample?
	disc_phase <- disc_phase_fn(osc_phase, ...)
	disc <- will_wrap(disc_phase, step)
	if (disc) debuglog(LOG_DISC, '-', label, ': disc', sample, '\n')

	post_sync_disc_phase <- 0
	post_sync_disc <- F

	if (!is.null(sync_info))
	{
		# If a discontinuity is also scheduled, it should be ignored if the sync occurs first.
		if (disc && sync_info$ttr < time_to_reset(disc_phase, step))
			disc <- F

		# Did the sync place the oscillator's phase right before a discontinuity?
		# This will be very rare for SYNC_RESET.
		post_sync_disc_phase <- disc_phase_fn(sync_info$adj_phase, ...)
		new_phase_at_sync <- disc_phase_fn(sync_info$new_phase_at_sync, ...)
		if (will_wrap(post_sync_disc_phase, step) && will_wrap(new_phase_at_sync, step))
		{
			post_sync_disc <- T
			debuglog(LOG_DISC, '-', label, ': post-sync', sample, '\n')

			# Not useful calculations. Just consistency checks.
			delta <- (disc_phase_fn(sync_info$phase_at_sync, ...) - disc_phase) %% 1 + (1 - new_phase_at_sync)
			stopifnot(delta > 0 && delta < step)
			second_phase <- 1 - delta
			stopifnot(all.equal(disc_phase_fn(sync_info$adj_phase, ...), second_phase))
		}
	}

	return (list(disc=disc,
	             disc_phase=disc_phase,
	             post_sync_disc=post_sync_disc,
	             post_sync_disc_phase=post_sync_disc_phase))
}

ramp <- function(phase)
{
	return (2 * phase - 1)
}

pulse <- function(phase, pw, dc_comp)
{
	wave <- ifelse(phase < pw, 1, -1)
	if (dc_comp) wave <- wave - 2 * (pw - 0.5)
	return (wave)
}

tri_pulse <- function(phase, pw, dc_comp)
{
	return (pulse(tri_pulse_up_phase(phase, pw), pw, dc_comp))
}

# Converts from [-1, 1] to [xrange[1], xrange[2]]
transform <- function(x, xrange)
{
	return((xrange[2] - xrange[1]) * (x + 1) / 2 + xrange[1])
}

triangle <- function(phase)
{
	return (1 - 2 * abs(ramp(phase)))
}

reset_phase <- function(phase)
{
	return (phase)
}

midpulse_phase <- function(phase, pw)
{
	return ((phase + (1 - pw)) %% 1)
}

tritop_phase <- function(phase)
{
	return (midpulse_phase(phase, 0.5))
}

tri_pulse_up_phase <- function(phase, pw)
{
	return ((phase + pw / 2) %% 1)
}

tri_pulse_down_phase <- function(phase, pw)
{
	return ((phase + 1 - pw / 2) %% 1)
}

will_wrap <- function(phase, step)
{
	return (phase > 1 - step)
}

time_to_reset <- function(phase, step)  # time to discontinuity, in samples.
{
	return ((1 - phase) / step)
}


SYNC_RESET <- 0
SYNC_REVERSE <- 1

SHAPE_TRI <- 0
SHAPE_RAMP <- 1
SHAPE_PULSE <- 2

# While there is a lot of logic here, the approach is simple at a high level.
# For each waveform:
# - Compute "naive" waveform.
# - Find all discontinuities occurring between the current and next
#   sample.
# - Apply corrections to the current sample (if applicable).
# - Store corrections to be applied to the next sample (if applicable).
#
# The bulk of the code deals with tracking all possible discontinuities,
# which gets complicated with oscillator sync.
osc <- function(samples, sample_rate, freq, sfreq, pw=0.5, sync=T, shape=SHAPE_RAMP, wav_range=c(-1, 1), pulse_dc_comp=F, pulse_from_tri=F, sync_type=SYNC_REVERSE)
{
	APPLY_NORM <- T
	APPLY_SYNC <- T
	APPLY_POST_SYNC <- T

	RAMP_RESET_JUMP <- -2
	PULSE_RESET_JUMP <- 2
	PULSE_FLIP_JUMP <- -2
	TRI_TOP <- 1
	TRI_BOTTOM <- -1

	result <- rep_len(0, samples)

	out_ramp <- SHAPE_RAMP %in% shape
	out_ramp_pulse <- SHAPE_PULSE %in% shape && !pulse_from_tri
	out_tri_pulse <- SHAPE_PULSE %in% shape && pulse_from_tri
	out_tri <- SHAPE_TRI %in% shape

	step <- freq / sample_rate
	phase <- 0#0.01
	sstep <- sfreq / sample_rate
	sphase <- 0#0.01

	rcorr <- 0
	pcorr <- 0
	tcorr <- 0

	for (i in 1:samples)
	{
		old_phase <- phase

		before_sync <- sync && will_wrap(sphase, sstep)
		if (before_sync) debuglog(LOG_DISC, 'Sync:', i, phase, sphase, '\n')

		sync_info <- NULL
		if (before_sync)
		{
			sync_ttr <- time_to_reset(sphase, sstep)

			phase_at_sync <- (phase + sync_ttr * step) %% 1
			if (sync_type == SYNC_RESET)
				new_phase_at_sync <- 0
			else if (sync_type == SYNC_REVERSE)
				new_phase_at_sync <- 1 - phase_at_sync  # Horizontal reflection around 0.5.
			else stopifnot(F)

			delta <- (phase_at_sync - old_phase) %% 1
			sync_phase <- 1 - delta
			phase <- (new_phase_at_sync - delta) %% 1

			sync_info <- list(
				ttr=sync_ttr,
				phase_at_sync=phase_at_sync,
				new_phase_at_sync=new_phase_at_sync,
				adj_phase=phase)
		}

		result[i] <- 0
		reset <- check_disc(old_phase, step, sync_info, 'reset', i, reset_phase)

		if (out_ramp)
		{
			r <- ramp(old_phase) + rcorr
			rcorr <- 0

			if (APPLY_NORM && reset$disc)
			{
				r <- r + poly_blep(reset$disc_phase, step, RAMP_RESET_JUMP)
				rcorr <- rcorr + poly_blep((reset$disc_phase + step) %% 1, step, RAMP_RESET_JUMP)
			}

			if (APPLY_SYNC && before_sync)
			{
				ramp_sync_jump <- ramp(new_phase_at_sync) - ramp(phase_at_sync)
				r <- r + poly_blep(sync_phase, step, ramp_sync_jump)
				rcorr <- rcorr + poly_blep((sync_phase + step) %% 1, step, ramp_sync_jump)
			}

			if (APPLY_POST_SYNC && reset$post_sync_disc)
			{
				r <- r + poly_blep(reset$post_sync_disc_phase, step, RAMP_RESET_JUMP)
				rcorr <- rcorr + poly_blep((reset$post_sync_disc_phase + step) %% 1, step, RAMP_RESET_JUMP)
			}

			result[i] <- result[i] + transform(r, wav_range)
		}

		if (out_tri_pulse)
		{
			reset <- check_disc(old_phase, step, sync_info, 'tri_pulse_up', i, tri_pulse_up_phase, pw=pw)
			flip <- check_disc(old_phase, step, sync_info, 'tri_pulse_down', i, tri_pulse_down_phase, pw=pw)

			p <- tri_pulse(old_phase, pw, pulse_dc_comp) + pcorr
			pcorr <- 0

			if (APPLY_NORM)
			{
				if (reset$disc)
				{
					p <- p + poly_blep(reset$disc_phase, step, PULSE_RESET_JUMP)
					pcorr <- pcorr + poly_blep((reset$disc_phase + step) %% 1, step, PULSE_RESET_JUMP)
				}
				if (flip$disc)
				{
					p <- p + poly_blep(flip$disc_phase, step, PULSE_FLIP_JUMP)
					pcorr <- pcorr + poly_blep((flip$disc_phase + step) %% 1, step, PULSE_FLIP_JUMP)
				}
			}

			if (APPLY_SYNC && before_sync)
			{
				pulse_sync_jump <- tri_pulse(new_phase_at_sync, pw, pulse_dc_comp) - tri_pulse(phase_at_sync, pw, pulse_dc_comp)
				if (sync_type == SYNC_REVERSE)
				{
					# The jump due to a sync is guaranteed to be 0 for a triangle-derived
					# pulse wave. So no correction is required.
					stopifnot(all.equal(pulse_sync_jump, 0))
				}
				else
				{
					p <- p + poly_blep(sync_phase, step, pulse_sync_jump)
					pcorr <- pcorr + poly_blep((sync_phase + step) %% 1, step, pulse_sync_jump)
				}
			}

			if (APPLY_POST_SYNC)
			{
				if (reset$post_sync_disc)
				{
					p <- p + poly_blep(reset$post_sync_disc_phase, step, PULSE_RESET_JUMP)
					pcorr <- pcorr + poly_blep((reset$post_sync_disc_phase + step) %% 1, step, PULSE_RESET_JUMP)
				}
				if (flip$post_sync_disc)
				{
					p <- p + poly_blep(flip$post_sync_disc_phase, step, PULSE_FLIP_JUMP)
					pcorr <- pcorr + poly_blep((flip$post_sync_disc_phase + step) %% 1, step, PULSE_FLIP_JUMP)
				}
			}

			result[i] <- result[i] + transform(p, wav_range)
		}

		if (out_ramp_pulse)
		{
			flip <- check_disc(old_phase, step, sync_info, 'midpulse', i, midpulse_phase, pw=pw)
			if (reset$disc && flip$disc) debuglog(LOG_DISC, '- Both flip and reset', i, '\n')

			p <- pulse(old_phase, pw, pulse_dc_comp) + pcorr
			pcorr <- 0

			if (APPLY_NORM)
			{
				if (reset$disc)
				{
					p <- p + poly_blep(reset$disc_phase, step, PULSE_RESET_JUMP)
					pcorr <- pcorr + poly_blep((reset$disc_phase + step) %% 1, step, PULSE_RESET_JUMP)
				}
				if (flip$disc)
				{
					p <- p + poly_blep(flip$disc_phase, step, PULSE_FLIP_JUMP)
					pcorr <- pcorr + poly_blep((flip$disc_phase + step) %% 1, step, PULSE_FLIP_JUMP)
				}
			}

			if (APPLY_SYNC && before_sync)
			{
				pulse_sync_jump <- pulse(new_phase_at_sync, pw, pulse_dc_comp) - pulse(phase_at_sync, pw, pulse_dc_comp)
				p <- p + poly_blep(sync_phase, step, pulse_sync_jump)
				pcorr <- pcorr + poly_blep((sync_phase + step) %% 1, step, pulse_sync_jump)
			}

			if (APPLY_POST_SYNC)
			{
				if (reset$post_sync_disc)
				{
					p <- p + poly_blep(reset$post_sync_disc_phase, step, PULSE_RESET_JUMP)
					pcorr <- pcorr + poly_blep((reset$post_sync_disc_phase + step) %% 1, step, PULSE_RESET_JUMP)
				}
				if (flip$post_sync_disc)
				{
					p <- p + poly_blep(flip$post_sync_disc_phase, step, PULSE_FLIP_JUMP)
					pcorr <- pcorr + poly_blep((flip$post_sync_disc_phase + step) %% 1, step, PULSE_FLIP_JUMP)
				}
			}

			result[i] <- result[i] + transform(p, wav_range)
		}

		if (out_tri)
		{
			# TODO: Only performing a 1-sample polyblamp correction. 2 samples
			#       would be better, but only makes a tiny difference and would
			#       complicate the sync handling code a lot.

			flip <- check_disc(old_phase, step, sync_info, 'tritop', i, tritop_phase)

			tri <- triangle(old_phase) + tcorr
			tcorr <- 0

			if (APPLY_NORM)
			{
				if (reset$disc)
				{
					tri <- tri + TRI_BOTTOM * poly_blamp(1 - reset$disc_phase, step)
					tcorr <- tcorr + TRI_BOTTOM * poly_blamp((reset$disc_phase + step) %% 1, step)
				}
				if (flip$disc)
				{
					tri <- tri + TRI_TOP * poly_blamp(1 - flip$disc_phase, step)
					tcorr <- tcorr + TRI_TOP * poly_blamp((flip$disc_phase + step) %% 1, step)
				}
			}

			if (APPLY_SYNC && before_sync)
			{
				if (sync_type == SYNC_REVERSE)
				{
					stopifnot(!reset$disc || !flip$disc)
					if (reset$disc)
						sync_dir <- TRI_TOP
					else if (flip$disc)
						sync_dir <- TRI_BOTTOM
					else
						sync_dir <- ifelse(old_phase < 0.5, TRI_TOP, TRI_BOTTOM)

					tri <- tri + sync_dir * poly_blamp(1 - sync_phase, step)
					tcorr <- tcorr + sync_dir * poly_blamp((sync_phase + step) %% 1, step)
				}
				else
				{
					stopifnot(sync_type == SYNC_RESET)
					sync_jump <- triangle(new_phase_at_sync) - triangle(phase_at_sync)
					tri <- tri + poly_blep(sync_phase, step, sync_jump)
					tcorr <- tcorr + poly_blep((sync_phase + step) %% 1, step, sync_jump)
				}
			}

			if (APPLY_POST_SYNC)
			{
				if (reset$post_sync_disc)
				{
					tri <- tri + TRI_BOTTOM * poly_blamp(1 - reset$post_sync_disc_phase, step)
					tcorr <- tcorr + TRI_BOTTOM * poly_blamp((reset$post_sync_disc_phase + step) %% 1, step)
				}
				if (flip$post_sync_disc)
				{
					tri <- tri + TRI_TOP * poly_blamp(1 - flip$post_sync_disc_phase, step)
					tcorr <- tcorr + TRI_TOP * poly_blamp((flip$post_sync_disc_phase + step) %% 1, step)
				}
			}

			result[i] <- result[i] + transform(tri, wav_range)
		}

		sphase <- (sphase + sstep) %% 1
		phase <- (phase + step) %% 1
	}

	return (result)
}

plot_freq <- function(w, zoom=F)
{
	n <- length(w)
	fs <- 48000
	idx <- 1:(n / 2)
	freqs <- 0:(n - 1) * fs / n
	ylim <- if (zoom) c(0,1000) else NULL
	plot(freqs[idx], abs(fft(w))[idx], type='l', ylim=ylim)
}

run_osc_test <- function(freq=1000, sfreq=800, samples=48000, fresh=T, zoom=F, ...)
{
	sample_rate <- 48000
	w <- osc(samples, sample_rate, freq, sfreq, ...)

	if (fresh) par(mfrow=c(1, 2))
	plot(w[1:min(100, samples)], type='b', cex=0.5, ylim=c(-1.1, 1.1))
	plot_freq(w, zoom=zoom)
}

run_all_osc_tests <- function(shape=SHAPE_RAMP, var=0, zoom=F, ...)
{
	par(mfrow=c(4, 2))

	pw <- 0.5
	pulse_from_tri <- F
	if (shape == SHAPE_TRI)
	{
		if (var == 0)
		{
			freq <- 2050
			sfreq <- c(1100, 1300, 1610, 2050)
		}
		else if (var == 1)
		{
			freq <- 2050
			sfreq <- c(2050, 2600, 3045, 4020)
		}
		else stopifnot(F)
	}
	else if (shape == SHAPE_RAMP)
	{
		if (var == 0)
		{
			freq <- 2600
			sfreq <- c(2300, 2400, 2500, 2600)
		}
		else stopifnot(F)
	}
	else if (shape == SHAPE_PULSE)
	{
		if (var == 0)
		{
			freq <- 2050
			sfreq <- c(1355, 1200, 1700, 2050)
		}
		else if (var == 1)
		{
			pw <- 0.25
			freq <- 2050
			sfreq <- c(1100, 1300, 1610, 2050)
		}
		else if (var == 2)
		{
			pulse_from_tri <- T
			freq <- 4000
			sfreq <- c(2500, 2266.667, 3133.333, 4200)
			# 2 and 3: test post-sync up and down transitions.
		}
		else if (var == 3)
		{
			pw <- 0.25
			pulse_from_tri <- T
			freq <- 3133.333
			sfreq <- c(2500, 2700, 3566.667, 4200)
			# 2 and 3: test post-sync up and down transition.
		}
		else stopifnot(F)
	}

	for (s in sfreq)
		run_osc_test(freq, s, pw=pw, shape=shape, pulse_from_tri=pulse_from_tri, zoom=zoom, fresh=F, ...)
}

# Intended to catch assertions.
run_exhaustive_tests <- function()
{
	shapes <- list(c(SHAPE_TRI, 0.5), c(SHAPE_RAMP, 0.5), c(SHAPE_PULSE, 0.5), c(SHAPE_PULSE, 0.2), c(SHAPE_PULSE, 0.75))
	freqs <- seq(from=100, to=4000, length.out=10)

	for (s in shapes)
	{
		for (f in freqs)
		{
			for (sf in freqs)
			{
				cat('TEST: ', s, f, sf, '\n')
				capture.output(
				{
					osc(48000, 48000, f, sf, sync=T, pw=s[2], shape=s[1])
				}, file=nullfile())
			}
		}
	}
}

