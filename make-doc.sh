echo build-doc
ocamlbuild random_generator.docdir/index.html

echo get-hash
HASH=`git rev-parse HEAD`

echo checkout gh-pages
git checkout gh-pages || exit 1

echo rm -fR doc
rm -fR doc

echo mv
mv _build/random_generator.docdir doc

git commit -a -m "regenerate documentation at commit $HASH"

echo checkout master
git checkout master
