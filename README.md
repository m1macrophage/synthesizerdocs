## Mission

Document, in the form of [MAME](https://github.com/mamedev/mame) drivers[^1], the digital and
digital-analog internals of classic music machines.
The focus is on digitally-controlled analog synthesizers and early digital devices.

Classic analog synthesizers, such as those of the 80s, are widely recognized and respected for their sound.
Their audio circuitry has been studied for decades, and a lot of information about that can be
found online. However, people often don't realize that many "analog" synthesizers are built around bespoke digital computers.

This effort aims to document the digital side of the story, and expose how the digital and analog work togeter.
It aspires to cover this oft-neglected part of computing and music history, in the hopes that people find it educational.

[^1]: These MAME drivers cannot replace the real thing, nor the myriad of commercial and free alternatives for these machines.
For starters, most of us can't run these emulations, since we don't have access to the firwmare.
And even if we could, important functionality (such as audio!) will be missing or lacking.
The work described here is a documentation of hisotrical facts. If you are interested in computer music history, or
want to take a glimpse at how these instruments worked, stick around and have a look at the driver source code.
If you are looking to make music, these drivers are not for you.


## Status

**Current focus**: Improving drivers below, extracting common functionality.

Status of other work:

| System | Digital | Digital-analog interface | Interactive layout | Full DC | Audio |
|---|---|---|---|---|---|
| [Moog Source](https://github.com/mamedev/mame/blob/master/src/mame/moog/source.cpp) | Done | Far along | Done | Not planned | Not planned |
| [Moog Memorymoog](https://github.com/mamedev/mame/blob/master/src/mame/moog/memorymoog.cpp) | Done | Far along | Not started | Not planned | Not planned |
| [Paia MIDI2CV8](https://github.com/mamedev/mame/blob/master/src/mame/paia/midi2cv8.cpp) | Done | Done | Done | Mostly done | N/A |
| [Paia Fatman](https://github.com/mamedev/mame/blob/master/src/mame/paia/fatman.cpp) | Done | Mostly done | Done | Early stage | Not planned |
| [Oberheim Xpander](https://github.com/m1macrophage/mamefork/blob/master/src/mame/oberheim/xpander.cpp) | Mostly done | Far along | Early stage | Not planned | Not planned |
| [Oberheim OB8](https://github.com/mamedev/mame/blob/master/src/mame/oberheim/ob8.cpp) | Early stage | Early stage | Not started | Not planned | Not planned |
| [Oberheim DMX](https://github.com/mamedev/mame/blob/master/src/mame/oberheim/dmx.cpp) (early version) | Done | Done | Done | Done | Done |
| [Linn LinnDrum](https://github.com/mamedev/mame/blob/master/src/mame/linn/linndrum.cpp) | Done | Far along | Done | Far along | Almost done |
| [Alesis MIDIverb](https://github.com/mamedev/mame/blob/master/src/mame/alesis/midiverb.cpp)  | Done | N/A | Done | N/A | Done |

Legend:
* **Digital**: Documentation state of the computer(s) used to to control the synth (UI, voice control). Does not include DSPs.
* **Digital-analog interface**: Documentation state of the analog circuitry that interacts with the *Digital* portion in some way. For example: LFOs and EGs whose state is accessed by the firmware, analog inputs and outputs, cessette I/O, autotune circuitry, etc.
* **Interactive layout**: A functional MAME layout. Planned for all devices.
* **Full DC**: Documentation of all audio control voltages and currents in the synthesizer, even those not read by the firmware. Only planned for digital-analog hybrids and simple analog synths.
* **Audio**: Documentation state of the audio circuitry. Planned for a few digital and digital-analog hybrid devices.
