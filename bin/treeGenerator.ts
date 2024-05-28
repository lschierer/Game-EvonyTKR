#!/usr/bin/env node

import * as path from 'path';
import * as process from 'process';
import { rimraf } from 'rimraf';
import {mkdirp} from 'mkdirp';

import {readAllFiles} from '../lib/pageMaker/utils';

console.log(`cwd is ${process.cwd()}`);

console.log(`project root is ${path.resolve(path.join(process.cwd(),'../..'))}`)

const dest = path.join(process.cwd(), 'pages/')

rimraf(dest);


const Proot =  path.resolve(path.join(process.cwd(),'../..')) + '/';
const doc_base = 'content/en/';
const source = path.join(process.cwd(),'../../content/en/')

const content_dirs = new Set<string>();

for (const file of readAllFiles(source)) {
  content_dirs.add(path.dirname(file.replace(Proot,'').replace(doc_base,'')));
}

content_dirs.forEach((d) =>{
  console.log(`to create ${dest + d}`);
  mkdirp(dest + d).then(made => {
    console.log(`created ${made}`);
  });
})
