#!/bin/sh

if [ ! -d "samples_dl" ]; then
    mkdir samples_dl
    cd samples_dl

    wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/TR909all.zip

    unzip TR909all.zip

    for file in *.WAV; do
        ffmpeg -i "$file" -ac 1 -ar 44100 -f s16le -acodec pcm_s16le "$file.wav"

        # This is the only solution I found to have raw samples without the WAV
        # header...
        dd bs=44 skip=1 if="$file.wav" of="$file.raw"
    done

    cd ..
fi

alr build

./bin/wnm_rom_builder
