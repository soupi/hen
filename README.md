Hen
---

A static blog generator


> License: Apache-2.0

> Status: Experimental


#### Site Structure

```sh
_site
├── _posts
│   ├── <post-name>.md
│   └── ...
├── side.md
└── static
    ├── css
    │   ├── normalize.css
    │   └── style.css
```

##### Post Format

```sh
route=<article-route>
title=<article title>
date=YYYY-MM-DD
---

<body content in markdown format>
```


##### Aside

`side.md`:

```sh
<whatever you want to display on the side>
```
