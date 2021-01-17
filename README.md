# [7 GUIs](https://eugenkiss.github.io/7guis/) in CLJS

[View Online](https://kurtharriger.github.io/seven-guis-cljs/docs/#/counter)

## Run locally
```bash
npm install
npx shadow-cljs watch frontend
```

## Publish release on github pages
```
git checkout gh-pages
git merge main
npm install 
rm -r docs
rm -r public/js
npx shadow-cljs release frontend
cp -r public docs

```

