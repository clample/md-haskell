# md-Haskell

A markdown to html parser written in Haskell.

Currently parses:
* header tags
* links
* images
* unordered lists
* paragraphs

## Header tags:
You can create header tags by starting a newline with `#` signs. The number of `#` signs gives the header level.
ex:
`# Header1` results in `<h1>Header1</h1>`
`# Header2` results in `<h2>Header2</h2>`

## Links
Links can be put in other elements, such as list elements or headers. Links are formed as follows:
`[Google](https://google.com)` results in `<a href="https://google.com">Google</a>`

## Images
Images are created similarly to links. They must be on their own line.
`![alt text](https://my.image.com)` results in `<img src="https://my.image.com" alt="alt text">`

## Unordered lists
Unordered lists are created by starting consecutive lines with `*`.
```
* List element 1
* List element 2
```
Results in
```
<ul>
<li>List element 1</li>
<li>List element 2</li>
</ul>

## Paragraphs
To create a paragraph tag, start text on it's own line. You can insert a `<br>` tag by including a single newline:
```
This is a paragraph
This is the second line
```
Results in
`<p>This is a paragraph<br>This is the second line</p>`