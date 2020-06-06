# web-page-segmentation-revisited
Code for "Web Page Segmentation Revisited: Evaluation Framework and Dataset", submitted as resources paper to CIKM 2020

```
# Fit to dom nodes
Rscript src/main/r/fit-to-dom-nodes.R --input webis-web-segments-20/000000/annotations.json --output webis-web-segments-20/000000/fitted-annotations.json

# Edge detection
./src/main/bash/detect-edges.sh webis-web-segments-20/000000/screenshot.png 2 1  # coarse
./src/main/bash/detect-edges.sh webis-web-segments-20/000000/screenshot.png 16 5 # fine
```


## Web Page Resources
If you want to create a similar dataset or want to run a segmenter in a browser, you should use the [webis-web-archiver](https://github.com/webis-de/webis-web-archiver).


### Create Similar Dataset
Use [archive.sh](https://github.com/webis-de/webis-web-archiver/blob/master/src-bash/archive.sh), which downloads a 2 GB docker image on first use, to create WARC files and other resources.

Our annotation interface's source code is contained in `src/main/html/hit-template.html`. If you want to test and analyze/adjust it locally (without using Mechanical Turk), open `src/main/html/hit-template-local.html` in your browser.


### Run In-browser Segmenter
This step is only necessary if the provided resources are not enough for a segmenter and they need access to a live rendering of a web page. Though no longer "live", you can still get high-fidelity reproductions of the web pages in our dataset using web archiving technology, just like we did for enriching the dataset with DOM node coordinates.

First write a custom script for the [webis-web-archiver](https://github.com/webis-de/webis-web-archiver) (see there) to run the segmenter during reproduction. This usually just involves to call the JavaScript code of the segmenter (you can use [this utility function](https://github.com/webis-de/webis-web-archiver/blob/master/src/de/webis/webarchive/environment/browsers/Windows.java#L49)) after [the page is loaded](https://github.com/webis-de/webis-web-archiver/blob/master/src/de/webis/webarchive/environment/scripts/ScrollDownScript.java#L49) and to store the output segments.

You can then use [reproduce.sh](https://github.com/webis-de/webis-web-archiver/blob/master/src-bash/reproduce.sh) with your script and the WARC file from the [webis-web-archive-17](https://webis.de/data.html#webis-web-archive-17) to get the segments.

