#!/bin/sh

source_file=$(ls */*.raw)

input_sample_rate=44100
input_format=f32le

output_sample_rate=44100

for src_path in ${source_file}; do
    src_file=$(basename ${src_path})
    src_stem=${src_file%.*}
    src_dir=$(dirname ${src_path})
    raw_dir=${src_dir}/raw_${output_sample_rate}
    wav_dir=${src_dir}/wav
    wav_path=${wav_dir}/${src_stem}.wav
    raw_path=${raw_dir}/${src_stem}.raw

    mkdir -p ${raw_dir}
    mkdir -p ${wav_dir}

    # First convert from RAW F32LE to WAV
    ffmpeg -y -f ${input_format} -ac 1 -ar ${input_sample_rate} -i "${src_path}" "${wav_path}"

    # Then to RAW S16LE
    ffmpeg -y -i "${wav_path}" -ac 1 -ar ${output_sample_rate} -f s16le -acodec pcm_s16le "resample.tmp"
    
    # This is the only solution I found to have raw samples without the WAV
    # header...
    dd bs=44 skip=1 if="resample.tmp" of=${raw_path}

done
