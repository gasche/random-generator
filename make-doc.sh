echo dune build @doc
dune build @doc

echo get-hash
HASH=`git rev-parse HEAD`

echo checkout gh-pages
git checkout gh-pages || exit 1

echo rm -fR doc
rm -fR doc

echo mv
mv _build/default/_doc/_html doc

# Add a redirect page from the "old" documentation page doc/Generator.html
# (referenced by the QCheck manual and maybe other places)
# to whatever is the right documentation URL these days.
cat <<EOF > doc/Generator.html
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Refresh" content="0; url=random-generator/Random_generator/index.html" />
  </head>
</html>
EOF

git add doc
git commit -a -m "regenerate documentation at commit $HASH"

echo checkout master
git checkout master
