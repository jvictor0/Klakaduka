#! /usr/bin/env python

from pysoundcard import Stream
from aubio import onset
from threading import *
import time
from math import *
import midi
import midi.sequencer as sequencer
from midi.events import NoteOnEvent
import json
import numpy.random

come_up_secs = 0.2 # number of seconds to gain energy from hit
nu = 0.5 # energy expand rate
eta = 0.05 # energy decay rate
epsilon = 0.01 # min energy

bpm = 180

client = 128
port = 0

energy = 0

def groove():

    global energy

    seq = sequencer.SequencerWrite()
    seq.subscribe_port(client, port)
    seq.start_sequencer()

    secs_per_beat = 60.0/bpm

    melody_dict = json.load(open('swing/time-melody-2voice.json'))
    system_dict = json.load(open('swing/time-system.json'))
    part_dict = [melody_dict,system_dict]

    system_voice = {"ride" : 51, "hat" : 44}
    melody_voice = {"voice1" : 38, "voice2" : 35}
    voice_dict = [melody_voice,system_voice]
    

    ticks_left = [0,0]

    signature = [12,3]

    secs_per_tick = secs_per_beat/12  #float(math.lcm(signature))

    loaded_parts = [[],[]]

    duration = [4,4]
    

    signature_conversion = [1,signature[0]/signature[1]]

    l_start = time.time()

    while True: 
        time.sleep(secs_per_tick - (time.time()-l_start)) # this will crash if loop takes too long!
        l_start = time.time()

        for i in range(2):
            if ticks_left[i] == 0:
                ticks_left[i], loaded_parts[i] = load_next_part(part_dict[i],voice_dict[i],energy,signature_conversion[i])
                print loaded_parts[i]

            ticks_left[i] -= 1
    
            for j in xrange(len(loaded_parts[i])):
                left, ix, voice, notes = loaded_parts[i][j]
                if left == 0:
                    emit(seq, voice, 200)                                            
                    loaded_parts[i][j][1] += 1 #ix
                    loaded_parts[i][j][0] = signature_conversion[i] * notes[ix+1] if ix+1 != len(notes) else -1 #left

                loaded_parts[i][j][0] -= 1 #left


def load_next_part(dict, voice_dict, energy, conversion):
    ix = numpy.random.binomial(len(dict["variations"])-1,max(epsilon,min(1-epsilon,energy)))
    part = dict["variations"][ix]
    parts = []
    ticks = dict["duration"] * dict["signature"] * conversion
    for voice, notes in part.items():
        if voice == "duration":
            ticks *= notes
        else:
            parts.append([notes[0]*conversion,0,voice_dict[voice],notes])
    return ticks, parts
    
    
def emit(seq,note,velocity):
    seq.event_write(NoteOnEvent(tick=0,channel=9,data=[note,velocity]),True)

def track_energy():
    """Track attacks of instrument, maintain global float energy"""

    global energy
    energy = 0.25

    win_size = 512                 # fft size
    hop_size = 256
    s = Stream(block_length = hop_size)

    o = onset("default", win_size, hop_size, s.sample_rate)


    come_up_steps = ceil(come_up_secs * s.sample_rate/hop_size)
    built_energy = 0
    built_steps = 0

    onsets = 0
    

    s.start()
    while True:
        vec = s.read(hop_size)
        # mix down to mono
        mono_vec = vec.sum(-1) / float(s.input_channels)
        if o(mono_vec):
            print "beat" + str(onsets)
            onsets += 1
            built_energy = (nu * (energy + built_steps * built_energy) + (1-nu) - energy)/(built_steps+come_up_steps)
            built_steps += come_up_steps
        if built_steps == 0 :
            energy = (1-eta) * energy
        else:
            energy += built_energy
            built_steps -= 1
#        print "energy = %f, total = %f" % (energy,energy + built_energy * built_steps)
    s.stop()

def start():
    Thread(None,track_energy).start()
    groove()

if __name__ == '__main__':
    start()
