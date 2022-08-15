# abort on errors
set -e

# build
npm run build

# navigate into the build output directory
cd dist

git init
git checkout -b main
git add -A
git commit -m 'deploy'

# deploying to https://<USERNAME>.github.io/<REPO>
git push -f git@github.com:lue-bird/where.git main:gh-pages

cd -