#!/bin/sh

if [ ! -d "samples_dl" ]; then
    mkdir samples_dl
    cd samples_dl

    wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/TR909all.zip

    unzip TR909all.zip

    for file in *.WAV; do
        ffmpeg -i "$file" -ac 1 -f s16le -acodec pcm_s16le "$file.raw"
    done

    cd ..
fi

alr build

./bin/wnm_rom_builder
