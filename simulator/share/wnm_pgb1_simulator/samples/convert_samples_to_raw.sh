#!/bin/sh

set -e

sample_rate=44100


waveform_picture () {
    ffmpeg -i "$1" -filter_complex "showwavespic=s=640x320:colors=black:split_channels=1" -frames:v 1 "$2"
}

# find . -name *.wav -type f -print0 | while IFS= read -r -d '' src_path; do
for src_path in `find . -name "*.wav" -type f`; do
    rm -f normalized.wav resample.tmp

    echo "========= ${src_path} =========="
    src_path=${PWD}/${src_path}
    src_file="$(basename ${src_path})"
    src_stem="${src_file%.*}"
    src_dir="$(dirname ${src_path})"
    raw_dir="${src_dir}/raw_${sample_rate}"
    raw_path="${raw_dir}/${src_stem}.raw"
    waveform_dir="${src_dir}/waveform_${sample_rate}"
    waveform_in_path="${waveform_dir}/${src_stem}-input.png"
    waveform_norm_path="${waveform_dir}/${src_stem}-norm.png"

    mkdir -p "${raw_dir}"
    # mkdir -p "${waveform_dir}"

    # waveform_picture "${src_path}" "${waveform_in_path}"

    ffmpeg-normalize "${src_path}" --normalization-type peak --target-level 0 -f -o "normalized.wav"

    # waveform_picture "normalized.wav" "${waveform_norm_path}"

    ffmpeg -y -i "normalized.wav" -ac 1 -ar ${sample_rate} -f s16le -acodec pcm_s16le "resample.tmp"
    
    # This is the only solution I found to have raw samples without the WAV
    # header...
    echo "write raw sample at ${raw_path}"
    dd bs=44 skip=1 if="resample.tmp" of="${raw_path}"
done
