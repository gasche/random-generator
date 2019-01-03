echo build-doc
ocamlbuild src/random_generator.docdir/index.html

echo get-hash
HASH=`git rev-parse HEAD`

echo checkout gh-pages
git checkout gh-pages || exit 1

echo rm -fR doc
rm -fR doc

echo mv
mv _build/src/random_generator.docdir doc

# We renamed Generator into Random_generator, add a redirect page
cat <<EOF > doc/Generator.html
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Refresh" content="0; url=Random_generator.html" />
  </head>
</html>
EOF

git add doc
git commit -a -m "regenerate documentation at commit $HASH"

echo checkout master
git checkout master
