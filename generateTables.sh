#! /usr/bin/env bash
# polynomials
#SAGE='/Users/drwells/Applications/sage/sage'
SAGE='sage'

for i in {1..10}
do
    $SAGE printTables.sage --order=$i --derivative=0 | grep '0' | \
    sed 's/\],/],\n/g' > Tables/polynomial_$i.txt
done

# gradients
for i in {1..10}
do
    $SAGE printTables.sage --order=$i --derivative=1 | grep '0' | \
    sed 's/\],/],\n/g' > Tables/gradient_$i.txt
done
