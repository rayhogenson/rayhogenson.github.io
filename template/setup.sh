#!/bin/sh

if [ -d template ]; then
	cd template
fi

root=..
template=$root/template
pages=$template/pages
raw=$template/raw

# Pages
for file in $(ls $pages); do
	sed -e "/{body}/{r $pages/$file
d}" $template/template.html > $root/$file
done

# Raw
for file in $(ls $raw); do
	ln -srf -t $root $file $raw/$file
done
