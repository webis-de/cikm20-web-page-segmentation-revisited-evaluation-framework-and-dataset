# Code for the CIKM20 Resources Paper: "Web Page Segmentation Revisited: Evaluation Framework and Dataset"
The code snippets of this README assume you unpacked the archives of the [dataset](https://webis.de/data.html?q=web-archive#webis-webseg-20) into this directory. For illustration, the code snippets below just use the web page `000000` (so you can just download the `webis-webseg-20-000000.zip` to try them out), but can be applied to all others in the same manner.
If you want to test a segmentation algorithm, read [Algorithm Evaluation](#algorithm-evaluation).
If you want to extend this dataset, create your own, or just check how we did it, read [Dataset Creation](#dataset-creation).


## Requirements
R (tested with 3.4.4)
  - jsonlite (tested with 1.6)
  - optparse (tested with 1.6.2)
  - png (tested with 0.1-7)
  - raster (tested with 2.9-23)
  - sf (tested with 0.7-7)
  - sp (tested with 1.3-1)

Relies on a local installation of `libgdal-dev` and `libudunits2-dev` (package names for Debian/Ubuntu).


## Algorithm Evaluation
The segmentation algorithm has to produce a segmentation in the same JSON format as the segmentations in this dataset. Then run:
```
Rscript src/main/r/evaluate-segmentation.R --algorithm <algorithm-segmentation.json> --ground-truth webis-webseg-20/000000/ground-truth.json
# Example: treat first fitted annotation as algorithm (ID of that annotation starts with 3I01)
Rscript src/main/r/evaluate-segmentation.R --algorithm webis-webseg-20/000000/fitted-annotations.json --algorithm-segmentation 3IO1 --ground-truth webis-webseg-20/000000/ground-truth.json
```

### Run In-browser Segmentation Algorithm
Some segmentation algorithms need access to a live rendering of a web page. Instead of running on a web page as it appears online at the moment, you can use web archiving technology to run the algorithm on archived web pages for a much better reproducibility:

First write a custom script for the [webis-web-archiver](https://github.com/webis-de/webis-web-archiver) (see there) to run the algorithm during reproduction. This usually just involves to call the JavaScript code of the algorithm (you can use [this utility function](https://github.com/webis-de/webis-web-archiver/blob/master/src/de/webis/webarchive/environment/browsers/Windows.java#L49)) after [the page is loaded](https://github.com/webis-de/webis-web-archiver/blob/master/src/de/webis/webarchive/environment/scripts/ScrollDownScript.java#L49) and to store the output segments.

You can then use [reproduce.sh](https://github.com/webis-de/webis-web-archiver/blob/master/src-bash/reproduce.sh) with your script and the WARC file from the [webis-web-archive-17](https://webis.de/data.html#webis-web-archive-17) to get the segments.



## Dataset Creation
You can use the [webis-web-archiver](https://github.com/webis-de/webis-web-archiver) to create web archives for a dataset that allows for reproducible in-browser segmentation.

Use [archive.sh](https://github.com/webis-de/webis-web-archiver/blob/master/src-bash/archive.sh), which downloads a 2 GB docker image on first use, to create WARC files and other resources.

Our annotation interface's source code is contained in `src/main/html/hit-template.html`. If you want to test and analyze/adjust it locally (without using Mechanical Turk), open `src/main/html/hit-template-local.html` in your browser.

Then do the following to reproduce our steps of dataset creation and agreement calculation:

### Edge Detection
We use image magick to detect edges. You can use it like this:
```
./src/main/bash/detect-edges.sh webis-webseg-20/000000/screenshot.png 1  2 # fine
mv webis-webseg-20/000000/screenshot-canny-0x1-1-2.png  webis-webseg-20/000000/screenshot-edges-fine.png
./src/main/bash/detect-edges.sh webis-webseg-20/000000/screenshot.png 5 16 # coarse
mv webis-webseg-20/000000/screenshot-canny-0x5-1-16.png webis-webseg-20/000000/screenshot-edges-coarse.png
```

### Fit to DOM Nodes
```
Rscript src/main/r/fit-segmentations-to-dom-nodes.R --input webis-webseg-20/000000/annotations.json --output webis-webseg-20/000000/fitted-annotations.json
```
Note: you can use the bash scripts [evaluate-fitting.sh](src/main/bash/evaluate-fitting.sh) and [get-empty-segments.sh](src/main/bash/get-empty-segments-file.sh) to process the standard output of the `fit-segmentations-to-dom-nodes.R` and to reproduce some analysis of the paper.

### Calculate Agreement
```
Rscript src/main/r/calculate-pairwise-agreement.R --input webis-webseg-20/000000/fitted-annotations.json --segmentations fitted --output webis-webseg-20/000000/agreement
tail webis-webseg-20/000000/agreement/*
Rscript src/main/r/calculate-averaged-agreement.R webis-webseg-20/000000/agreement/*
```

### Fuse Segmentations
```
min_annotators=3
disagreement_threshold=0.5
Rscript src/main/r/fuse-segmentations.R --input webis-webseg-20/000000/fitted-annotations.json --segments-min-annotators $min_annotators --disagreement-threshold=$disagreement_threshold --output webis-webseg-20/000000/ground-truth.json
```

### Plotting Segmentations
See the command help for all parameters.
```
Rscript src/main/r/plot-segmentations.R --input webis-webseg-20/000000/annotations.json --frames --line-width 5 --screenshot webis-webseg-20/000000/screenshot-edges-coarse.png --output annotations-on-coarse-edges.png
```


## Other Tools

### Combine Segmentation Files
Creates one segmentation file that contains all segmentations of the input segmentation files (which must all have the same `id`).
```
Rscript src/main/r/combine-segmentation-files.R segmentations1.json segmentations2.json segmentations-combined.json
```

### Calculate Statistics
Currently calculates the following statistics for each segmentation:
  - Number of segments
  - Ratio of pixels of the screenshot that are in at least one segment (by all pixels)

```
Rscript src/main/r/calculate-segmentations-statistics.R --input webis-webseg-20/000000/annotations.json
```

### Flatten Segmentations
Converts a hierarchical segmentation to a flat one by intersecting each segment with each other one.
```
Rscript src/main/r/flatten-segmentations.R --help
```


