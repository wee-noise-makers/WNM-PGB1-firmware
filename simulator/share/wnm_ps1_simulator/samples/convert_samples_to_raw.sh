#!/bin/sh

source_file=$(find . -name *.wav)

sample_rate=44100

for src_path in ${source_file}; do
    src_file=$(basename ${src_path})
    src_stem=${src_file%.*}
    src_dir=$(dirname ${src_path})
    raw_dir=${src_dir}/raw_${sample_rate}
    raw_path=${raw_dir}/${src_stem}.raw

    mkdir -p ${raw_dir}

    ffmpeg -y -i "$src_path" -ac 1 -ar ${sample_rate} -f s16le -acodec pcm_s16le "resample.tmp"
    
    # This is the only solution I found to have raw samples without the WAV
    # header...
    dd bs=44 skip=1 if="resample.tmp" of=${raw_path}

done
