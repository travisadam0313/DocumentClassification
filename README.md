# DocumentClassification
Document Classification via OCR using R

This work up demonstrates how documents may be classified using the Tesseract package in R.

First, 2 sample images of each document type are OCR'd and parsed into text tables.

A text table contains each individual string OCR'd from the document separated by row with a count of occurrences the string was found in the scan. Then the values are multiplied by -1

Example of table format:
Text  | Count
and   | -2
the   | -4
award | -1

After these tables are created and staged, a target image will go through the same process without the -1 multiplier.
The target text table is then merged with the training text tables and a sum of the common words is produced in the Z column.

When an exact count of words occurs, the Z column will have a 0. These zero rows out of total rows in the text tables are what creates the score for a document match.

The scores are then aggregated up by document type and the final output will provide the Document Type, Score, Index (Used in larger version of this process that scours document sets) and File Name.

Additional code is provided at the bottom to produce the output plot provided.
