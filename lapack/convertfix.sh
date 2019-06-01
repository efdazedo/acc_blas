cp $1 $1.bak
sed "s/^\*/!/" < $1 >/tmp/$1
cp /tmp/$1 $1
