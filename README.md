F♭ V.0.0.1
==========

## A programming language aimed at music composition.

This directory is an implementation of the programming language F♭.
A music-composition DSL, designed and implemented during the final
14-day mini-project at a course named `Programming Language Design`,
at DIKU in 2016.

## Trying out the language.

Assuming that you are on a Debian-based linux distro:

 1. Check that your machine has a group for playing audio by typing:
    > `grep audio /etc/group`  
    look for something that looks like:  
    > `audio:x:29:pulse`  
    And add yourself to the group by typing :  
    > `sudo gpasswd -a` **yourUserName** `audio`
 2. Install a music-synthesis or sequencer program. (I suggest fluidsynth).
    > `sudo apt-get install fluidsynth`  
    > `sudo apt-get install fluid-soundfont-gm`  
    > `sudo apt-get install jackd2`  
    > `sudo apt-get install alsa`
 3. Install F# (a functional language) by typing:
    > `sudo apt-get instal mono-complete`
 4. Install pymidi by typing:
    > `sudo apt-gte install python-midiutil`
 5. Start up a the synthesiser by entering:
    > `fluidsynth --audio-driver=alsa -o audio.alsa.device=hw:0 /usr/share/sounds/sf2/FluidR3_GM.sf2`  
    in a seperate terminal.
 6. Change directory into the directory `./bin` and run the script named `tests.sh`

## Future work.

A version V.1.0.0 of F♭ will be later this summer.
This first version will not be backwards compatible with V.0.0.1.