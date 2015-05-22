htdp
====

Script for rendering a version of How To Design Programs.
Running `main.rkt` invokes the renderer and generates an HTML document from the scribble sources in `base/`.

Files are:
- `x-info.rkt` paths and helper functions for rendering
- `xnotes.rkt` functions for compiling the notes
- `xhtml.rkt` functions for compiling the main textbook
- `main.rkt` invokes `xhtml.rkt` to render the document

This benchmark was proposed because it uses mixins, but they actually play a very small role.
They pass untouched through the typed (and untyped) code.


Experience Porting typed/HTDP
-----------------------------

1. Function `render` from `scribble/render` takes a class for the `#:render-mixin` option.
   This class is documented as extending/implementing `render<%>`. [docs](http://www.cs.utah.edu/plt/snapshots/current/doc/scribble/renderer.html?q=interface#%28def._%28%28lib._scribble%2Ftext-render..rkt%29._render-mixin%29%29)
2. Typed Racket's `Instance` type takes a class type. `render<%>` is not a class type.
3. Typed Racket's `Class #:implements` takes a class type. Again, `render<%>` is not a class type.
4. Can I not turn a class into a class type automatically?
5. Asumu says no, need to define the "Class type" myself.
6. Turns out missing method `traverse-paragraph` exists but is undocumented.
   Wait, missing method keeps changing names.
   - Now it's `get-serialize-version`.
   - Now it's `skip-extra-file`.
   - Now it's `traverse-block`.
   - Now it's `resolve` [doc](http://www.cs.utah.edu/plt/snapshots/current/doc/scribble/renderer.html?q=resolve#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._resolve%29%29)
   - Now it's `extract-pretitle`
   - Now it's `get-suffix`
   - `table-of-contents` (not documented for render interface)
   - `index-manual-newlines?`
   - `render` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=table-of-contents#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._render%29%29)
   - `extract-version`
   - `path->root-relative`
   - `local-table-of-contents`
   - `number-depth`
   - `report-output?`
   - `traverse-table`
   - `report-output!`
   - `set-external-tag-path` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=report-output%3F#%28meth._%28%28%28lib._scribble%2Fhtml-render..rkt%29._render-mixin%29._set-external-tag-path%29%29)
   - `deserialize-info` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=report-output%3F#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._deserialize-info%29%29)
   - `collect-flow`
   - `format-number`
   - `set-external-root-url` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=collect-flow#%28meth._%28%28%28lib._scribble%2Fhtml-render..rkt%29._render-mixin%29._set-external-root-url%29%29)
   - `add-extra-script-file`
   - `set-directory-depth` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=add-extra-script-file#%28meth._%28%28%28lib._scribble%2Fhtml-render..rkt%29._render-multi-mixin%29._set-directory-depth%29%29)
   - `get-dest-directory`
   - `resolve-content`
   - `render-block`
   - `string-to-implicit-styles`
   - `start-traverse`
   - `sort-image-requests`
   - `get-substitutions`
   - `extract-date`
   - `traverse-flow`
   - `current-render-mode`
   - `root-relative->path`
   - `install-extra-files`
   - `extract-authors`
   - `root-relative?`
   - `quiet-table-of-contents`
   - `render-other`
   - `serialize-info` [doc](http://plt.eecs.northwestern.edu/snapshots/current/doc/scribble/renderer.html?q=root-relative-%3Epath#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._serialize-info%29%29)
   - `fresh-tag-collect-context?`
   - `serialize-one-ht`
   - `collect-index-element`
   - `fresh-tag-resolve-context?`
   - `fresh-tag-render-context?`
   - `get-serialize-version`
   - `resolve-flow`
   - `collect` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._collect%29%29)
   - `render-auxiliary-table`
   - `render-itemization`
   - `render-intrapara-block`
   - `get-defined`
   - `serialize-infos` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._serialize-infos%29%29)
   - `render-paragraph`
   - `get-defineds` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._get-defineds%29%29)
   - `transfer-info`
   - `traverse-part`
   - `traverse-paragraph`
   - `get-external` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._get-external%29%29)
   - `traverse-itemization`
   - `traverse-nested-flow`
   - `collect-part`
   - `traverse-compound-paragraph`
   - `collect-itemization`
   - `render-table`
   - `start-collect`
   - `resolve-table`
   - `render-content`
   - `render-flow`
   - `start-resolve`
   - `render-compound-paragraph`
   - `collect-part-tags`
   - `collect-table`
   - `render-part-content`
   - `get-undefined` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._get-undefined%29%29)
   - `collect-paragraph`
   - `collect-block`
   - `resolve-block`
   - `render-one`
   - `render-part`
   - `traverse` [doc](http://docs.racket-lang.org/scribble/renderer.html?q=collect#%28meth._%28%28%28lib._scribble%2Fbase-render..rkt%29._render~3c~25~3e%29._traverse%29%29)
   - `collect-nested-flow`
   - `collect-compound-paragraph`
   - `resolve-paragraph`
   - `resolve-part`
   - `collect-content`
   - `collect-target-element`
   - `resolve-itemization`
   - `resolve-nested-flow`
   - `resolve-compound-paragraph`
   - `auto-extra-files?`
   - `auto-extra-files-paths`
7. Error: missing more methods, these belong to html-render.
   But the methods aren't in the render interface so there are errors.
8. Tried changing all `RenderMixin` occurrences with `(All (A B) (-> A B))`.
   Got error "superclass expression: result is not a class"
9. With Asumu: made a row polymorphic type. Things are great now.
   Only typed the documented `render%` methods.
   Unfortunately the mixin doesn't work because it adds undocumented methods.
