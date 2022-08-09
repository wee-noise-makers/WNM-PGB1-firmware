#!/bin/sh

mkdir samples_dl
cd samples_dl

wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/TR909all.zip

unzip TR909all.zip

for file in *.WAV; do ffmpeg -i "$file" -ac 1 -f s16le -acodec pcm_s16le "$file.raw"; done

mkdir /tmp/wnm-samples
cp *.raw /tmp/wnm-samples/
