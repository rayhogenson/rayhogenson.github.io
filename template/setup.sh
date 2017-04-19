#!/bin/sh

if [ -d template ]; then
	cd template
fi

root=..
template=$root/template
pages=$template/pages
posts=$template/posts
raw=$template/raw

# Blog
cat $template/post_header.html > $pages/blog.html

for file in $(ls $posts | sort -r); do
	echo '<h2>' >> $pages/blog.html
	echo $file | sed -e 's/\.md//' -e 's/_/ /g' >> $pages/blog.html
	echo '</h2>' >> $pages/blog.html
	cmark $posts/$file >> $pages/blog.html
done

# Pages
for file in $(ls $pages); do
	sed -e "/{body}/{r $pages/$file
d}" $template/template.html > $root/$file
done

# Raw
for file in $(ls $raw); do
	ln -srf -t $root $file $raw/$file
done
