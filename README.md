# What
Poi is a recycle bin for file heavily inspired from [trash-cli](https://github.com/andreafrancia/trash-cli).

# Usage
Please run `poi help`.

# Operation
## ListUp
`poi listup` shows you Poi's trash can.

Trashed file list has an index number which is for identifing a target file in some operations.

## Toss
`poi toss` moves a file to Poi's trash can from an original path.

Tossed files are able to back to orignal path.

## PickUp
`poi pickup` backs a file to an original path.

If backing file names were conflicted, Poi provides numbering suffix to the backing one.

You can specified trashed file index directly.

Please run `poi help` about detail.

## Bury
`poi bury` delete a file permanently.

This operation makes Poi's trash can empty default, but you can specified the target trashed term.

Please run `poi help` about detail.
