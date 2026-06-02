# license:BSD-3-Clause-Clear
# copyright-holders:m1macrophage

# Proof-of-concept implementation of a band-limitted oscillator, including
# support for sync. Intended for experimentation and verification of the
# relevant algorithms.

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

ramp <- function(phase)
{
	return (2 * phase - 1)
}

pulse <- function(phase, pw)
{
	return (ifelse(phase < pw, 1, -1))
}

triangle <- function(phase)
{
	return (1 - 2 * abs(ramp(phase)))
}

pw_phase <- function(phase, pw)
{
	return ((phase + (1 - pw)) %% 1)
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
SYNC_FLIP <- 1

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
osc <- function(samples, sample_rate, freq, sfreq, pw=0.5, sync=T, shape=SHAPE_RAMP, sync_type=SYNC_FLIP)
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
		post_sync_reset <- F

		before_sync <- sync && will_wrap(sphase, sstep)
		if (before_sync) debuglog(LOG_DISC, 'Sync:', i, phase, sphase, '\n')

		before_reset <- F
		if (will_wrap(old_phase, step))
		{
			before_reset <- T
			debuglog(LOG_DISC, 'Reset:', i, old_phase, sphase, '\n')
		}

		if (before_sync)
		{
			sync_ttr <- time_to_reset(sphase, sstep)

			if (before_reset)
			{
				if (sync_ttr < time_to_reset(old_phase, step)) { before_reset <- F }
				else { debuglog(LOG_DISC, '- Overlapping reset', i, '\n') }
			}

			phase_at_sync <- (phase + sync_ttr * step) %% 1
			if (sync_type == SYNC_RESET)
			{
				new_phase_at_sync <- 0
				sync_phase <- 1 - (phase_at_sync - phase) %% 1
				phase <- sync_phase
			}
			else if (sync_type == SYNC_FLIP)
			{
				# A reflection around 0.5.
				new_phase_at_sync <- 1 - phase_at_sync
				delta <- (phase_at_sync - phase) %% 1
				sync_phase <- 1 - delta
				phase <- (new_phase_at_sync - delta) %% 1
			}

			# Technically only needed for SYNC_FLIP, but exercising logic in SYNC_RESET too.
			if (will_wrap(phase, step) && will_wrap(new_phase_at_sync, step))
			{
				debuglog(LOG_DISC, '- Post-sync reset', i, '\n')
				delta <- (phase_at_sync - old_phase) %% 1 + (1 - new_phase_at_sync)
				stopifnot(delta > 0 && delta < step)
				second_phase <- 1 - delta
				stopifnot(all.equal(phase, second_phase))
				post_sync_reset <- T
			}
		}

		if (shape == SHAPE_RAMP)
		{
			r <- ramp(old_phase) + rcorr
			rcorr <- 0

			if (before_sync)
			{
				if (APPLY_SYNC)
				{
					ramp_sync_jump <- ramp(new_phase_at_sync) - ramp(phase_at_sync)
					r <- r + poly_blep(sync_phase, step, ramp_sync_jump)
					rcorr <- rcorr + poly_blep((sync_phase + step) %% 1, step, ramp_sync_jump)
				}

				if (APPLY_POST_SYNC && post_sync_reset)
				{
					r <- r + poly_blep(phase, step, RAMP_RESET_JUMP)
					rcorr <- rcorr + poly_blep((phase + step) %% 1, step, RAMP_RESET_JUMP)
				}
			}

			if (APPLY_NORM && before_reset)
			{
				r <- r + poly_blep(old_phase, step, RAMP_RESET_JUMP)
				rcorr <- rcorr + poly_blep((old_phase + step) %% 1, step, RAMP_RESET_JUMP)
			}

			result[i] <- r
		}
		else if (shape == SHAPE_PULSE)
		{
			p <- pulse(old_phase, pw) + pcorr
			pcorr <- 0

			midpulse_phase <- pw_phase(old_phase, pw)

			before_midpulse <- F
			if (will_wrap(midpulse_phase, step))
			{
				before_midpulse <- T
				debuglog(LOG_DISC, 'Pulse flip:', i, old_phase, sphase, '\n')
			}

			if (before_reset && before_midpulse) debuglog(LOG_DISC, '- Both flip and reset', i, '\n')

			if (before_sync)
			{
				if (before_midpulse)
				{
					if (sync_ttr < time_to_reset(midpulse_phase, step)) { before_midpulse <- F }
					else { debuglog(LOG_DISC, '- Overlaping pulse flip', i, ' - ', phase, midpulse_phase, '\n') }
				}

				# Only really needed for SYNC_FLIP
				post_sync_midpulse <- F
				if (will_wrap(pw_phase(phase, pw), step) && will_wrap(pw_phase(new_phase_at_sync, pw), step))
				{
					debuglog(LOG_DISC, '- Post-sync pulse flip', i, '\n')
					delta <- (pw_phase(phase_at_sync, pw) - pw_phase(old_phase, pw)) %% 1 + (1 - pw_phase(new_phase_at_sync, pw))
					stopifnot(delta > 0 && delta < step)
					second_pulse_phase <- 1 - delta
					stopifnot(all.equal(pw_phase(phase, pw), second_pulse_phase))
					post_sync_midpulse <- T
				}

				if (APPLY_SYNC)
				{
					pulse_sync_jump <- pulse(new_phase_at_sync, pw) - pulse(phase_at_sync, pw)
					p <- p + poly_blep(sync_phase, step, pulse_sync_jump)
					pcorr <- pcorr + poly_blep((sync_phase + step) %% 1, step, pulse_sync_jump)
				}

				if (APPLY_POST_SYNC)
				{
					if (post_sync_reset)
					{
						p <- p + poly_blep(phase, step, PULSE_RESET_JUMP)
						pcorr <- pcorr + poly_blep((phase + step) %% 1, step, PULSE_RESET_JUMP)
					}
					if (post_sync_midpulse)
					{
						second_pulse_phase <- pw_phase(phase, pw)
						p <- p + poly_blep(second_pulse_phase, step, PULSE_FLIP_JUMP)
						pcorr <- pcorr + poly_blep((second_pulse_phase + step) %% 1, step, PULSE_FLIP_JUMP)
					}
				}
			}

			if (APPLY_NORM)
			{
				if (before_reset)
				{
					p <- p + poly_blep(old_phase, step, PULSE_RESET_JUMP)
					pcorr <- pcorr + poly_blep((old_phase + step) %% 1, step, PULSE_RESET_JUMP)
				}
				if (before_midpulse)
				{
					p <- p + poly_blep(midpulse_phase, step, PULSE_FLIP_JUMP)
					pcorr <- pcorr + poly_blep((midpulse_phase + step) %% 1, step, PULSE_FLIP_JUMP)
				}
			}

			result[i] <- p
		}
		else if (shape == SHAPE_TRI)
		{
			# TODO: Only performing a 1-sample polyblamp correction. 2 samples
			#       would be better, but only makes a tiny difference and would
			#       complicate the sync handling code a lot.

			tri <- triangle(old_phase) + tcorr
			tcorr <- 0

			tritop_phase <- pw_phase(old_phase, 0.5)

			before_tritop <- F
			if (will_wrap(tritop_phase, step))
			{
				before_tritop <- T
				debuglog(LOG_DISC, 'Triangle flip:', i, old_phase, sphase, '\n')
			}

			if (before_sync)
			{
				if (before_tritop)
				{
					if (sync_ttr < time_to_reset(tritop_phase, step)) { before_tritop <- F }
					else { debuglog(LOG_DISC, '- Overlapping triangle flip', i, ' - ', old_phase, tritop_phase, '\n') }
				}

				# Technically only needed for SYNC_FLIP.
				post_sync_tritop <- F
				if (will_wrap(pw_phase(phase, 0.5), step) && will_wrap(pw_phase(new_phase_at_sync, 0.5), step))
				{
					debuglog(LOG_DISC, '- Post-sync triangle flip', i, '\n')
					delta <- (pw_phase(phase_at_sync, 0.5) - pw_phase(old_phase, 0.5)) %% 1 + (1 - pw_phase(new_phase_at_sync, 0.5))
					stopifnot(delta > 0 && delta < step)
					second_tritop_phase <- 1 - delta
					stopifnot(all.equal(pw_phase(phase, 0.5), second_tritop_phase))
					post_sync_tritop <- T
				}

				if (APPLY_SYNC)
				{
					stopifnot(!before_reset || !before_tritop)
					if (before_reset) { sync_dir <- TRI_TOP }
					else if (before_tritop) { sync_dir <- TRI_BOTTOM }
					else { sync_dir <- ifelse(old_phase < 0.5, TRI_TOP, TRI_BOTTOM) }

					tri <- tri + sync_dir * poly_blamp(1 - sync_phase, step)
					tcorr[1] <- tcorr[1] + sync_dir * poly_blamp((sync_phase + step) %% 1, step)
				}

				if (APPLY_POST_SYNC)
				{
					if (post_sync_reset)
					{
						tri <- tri + TRI_BOTTOM * poly_blamp(1 - phase, step)
						tcorr[1] <- tcorr[1] + TRI_BOTTOM * poly_blamp((phase + step) %% 1, step)
					}
					if (post_sync_tritop)
					{
						second_tritop_phase <- pw_phase(phase, 0.5)
						tri <- tri + TRI_TOP * poly_blamp(1 - second_tritop_phase, step)
						tcorr[1] <- tcorr[1] + TRI_TOP * poly_blamp((second_tritop_phase + step) %% 1, step)
					}
				}
			}

			if (APPLY_NORM)
			{
				if (before_reset)
				{
					tri <- tri + TRI_BOTTOM * poly_blamp(1 - old_phase, step)
					tcorr[1] <- tcorr[1] + TRI_BOTTOM * poly_blamp((old_phase + step) %% 1, step)
				}
				if (before_tritop)
				{
					tri <- tri + TRI_TOP * poly_blamp(1 - tritop_phase, step)
					tcorr[1] <- tcorr[1] + TRI_TOP * poly_blamp((tritop_phase + step) %% 1, step)
				}
			}

			result[i] <- tri
		}
		else
		{
			stopifnot(F)
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

	if (fresh) { par(mfrow=c(1, 2)) }
	plot(w[1:min(100, samples)], type='b', cex=0.5, ylim=c(-1.1, 1.1))
	plot_freq(w, zoom=zoom)
}

run_all_osc_tests <- function(shape=SHAPE_RAMP, zoom=F)
{
	par(mfrow=c(4, 2))

	pw <- 0.5
	if (shape == SHAPE_TRI)
	{
		freq <- 2050
		sfreq <- c(1100, 1300, 1610, 2050)
	}
	else if (shape == SHAPE_TRI + 10)
	{
		freq <- 2050
		sfreq <- c(2050, 2600, 3045, 4020)
	}
	else if (shape == SHAPE_RAMP)
	{
		freq <- 2600
		sfreq <- c(2300, 2400, 2500, 2600)
	}
	else if (shape == SHAPE_PULSE)
	{
		freq <- 2050
		sfreq <- c(1355, 1200, 1700, 2050)
	}
	else if (shape == SHAPE_PULSE + 1)
	{
		pw <- 0.25
		freq <- 2050
		sfreq <- c(1100, 1300, 1610, 2050)
	}

	if (shape == 3) { shape <- 2 }
	if (shape >= 10) { shape <- shape - 10 }
	for (s in sfreq)
	{
		run_osc_test(freq, s, pw=pw, shape=shape, zoom=zoom, fresh=F)
	}
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

