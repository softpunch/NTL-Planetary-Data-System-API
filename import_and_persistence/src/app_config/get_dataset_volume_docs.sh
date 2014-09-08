#!/bin/bash

for i in `seq -w 1 18`
do

VOL_NAME=LROLRC_10$i

mkdir $VOL_NAME
pushd $VOL_NAME > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/AAREADME.TXT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/VOLDESC.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/ERRATA.TXT > /dev/null

mkdir CATALOG
pushd CATALOG > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/DATASET.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/MISSION.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/PERSON.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/REF.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/INSTHOST.CAT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/CATINFO.TXT > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-3-CDR-V1.0/$VOL_NAME/CATALOG/INST.CAT > /dev/null

popd > /dev/null

# the code is hell-bent on finding lower case file names. let's lowercase all the files.
for SRC in `find . -depth`
do
    DST=`dirname "${SRC}"`/`basename "${SRC}" | tr '[A-Z]' '[a-z]'`
    if [ "${SRC}" != "${DST}" ]
    then
        [ ! -e "${DST}" ] && mv -T "${SRC}" "${DST}" || echo "${SRC} was not renamed"
    fi
done

popd > /dev/null

done
