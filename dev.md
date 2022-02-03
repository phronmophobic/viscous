## Dev notes

To update web demo, run the following from the root dir:

```sh
# clean first
rm -rf .shadow-cljs/builds/
clj -M:shadow-cljs release app
```

This will write the js file to `public/js/main.js`.

```sh
cp public/js/main.js docs/js/main.js
```

