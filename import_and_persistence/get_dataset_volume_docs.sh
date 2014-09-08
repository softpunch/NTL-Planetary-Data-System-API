#!/bin/bash

mkdir lroc-edr-dataset-docs
pushd lroc-edr-dataset-docs > /dev/null

for i in `seq -w 1 18`
do

VOL_NAME=LROLRC_00$i

mkdir $VOL_NAME
pushd $VOL_NAME > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/AAREADME.TXT -O aareadme.txt > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/VOLDESC.CAT -O voldesc.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/ERRATA.TXT -O errata.txt > /dev/null

mkdir catalog
pushd catalog > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/DATASET.CAT -O dataset.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/MISSION.CAT -O mission.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/PERSON.CAT -O person.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/REF.CAT -O ref.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/INSTHOST.CAT -O insthost.cat > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/CATINFO.TXT -O catinfo.txt > /dev/null
wget http://lroc.sese.asu.edu/data/LRO-L-LROC-2-EDR-V1.0/$VOL_NAME/CATALOG/INST.CAT -O inst.cat > /dev/null

popd > /dev/null

popd > /dev/null

done

popd
