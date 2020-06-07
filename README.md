# web-page-segmentation-revisited
Code for "Web Page Segmentation Revisited: Evaluation Framework and Dataset", submitted as resources paper to CIKM 2020.

If you want to test a segmentation algorithm, read [Algorithm Evaluation](#algorithm-evaluation).
If you want to extend this dataset, create your own, or just check how we did it, read [Dataset Creation](#dataset-creation).



## Algorithm Evaluation
The segmentation algorithm has to produce a segmentation in the same JSON format as the segmentations in this dataset. Then run:
```
Rscript src/main/r/evaluate-segmentation.R --algorithm <algorithm-segmentation.json> --ground-truth webis-web-segments-20/000000/ground-truth.json
# Example: treat first fitted annotation as algorithm (ID of that annotation starts with 3I01)
Rscript src/main/r/evaluate-segmentation.R --algorithm webis-web-segments-20/000000/fitted-annotations.json --algorithm-segmentation 3IO1 --ground-truth webis-web-segments-20/000000/ground-truth.json
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
./src/main/bash/detect-edges.sh webis-web-segments-20/000000/screenshot.png 1  2 # fine
mv webis-web-segments-20/000000/screenshot-canny-0x1-1-2.png  webis-web-segments-20/000000/screenshot-edges-fine.png
./src/main/bash/detect-edges.sh webis-web-segments-20/000000/screenshot.png 5 16 # coarse
mv webis-web-segments-20/000000/screenshot-canny-0x5-1-16.png webis-web-segments-20/000000/screenshot-edges-coarse.png
```

### Fit to DOM Nodes
```
Rscript src/main/r/fit-to-dom-nodes.R --input webis-web-segments-20/000000/annotations.json --output webis-web-segments-20/000000/fitted-annotations.json
```

### Calculate Agreement Matrices
```
Rscript src/main/r/calculate-agreement.R --input webis-web-segments-20/000000/fitted-annotations.json --segmentations fitted --output webis-web-segments-20/000000/agreement
tail webis-web-segments-20/000000/agreement/*
```

### Fuse Segmentations
```
min_annotators=3
disagreement_threshold=0.5
Rscript src/main/r/fuse-segmentations.R --input webis-web-segments-20/000000/fitted-annotations.json --segments-min-annotators $min_annotators --disagreement-threshold=$disagreement_threshold --output webis-web-segments-20/000000/ground-truth.json
```



