#!/bin/bash

image="heat_"
movie="heat"

echo "Using ffmpeg to create $movie.mp4 from $image[xxx].png"
ffmpeg -r 5 -f image2 -i $image%04d0.png -f mp4 -q:v 0 -vcodec mpeg4 -r 20 $movie.mp4
