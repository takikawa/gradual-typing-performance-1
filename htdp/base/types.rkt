#lang typed/racket

(provide Tag
         Block
         part
         RenderClass
         RenderMixin)

;; -----------------------------------------------------------------------------

(require/typed
 scribble/core
 [#:opaque Tag tag?]
 [#:opaque Block block?]
 [#:struct part ([tag-prefix : (U #f String)]
                 [tags : (Listof Tag)]
                 [title-content : (U #f (Listof Any))]
                 [style : Any] ;; "A style property can be anything, including a symbol or a structure such as color-property." http://docs.racket-lang.org/scribble/core.html?q=scribble%2Frender#%28tech._style._property%29
                 [to-collect : (Listof Any)]
                 [blocks : (Listof Block)]
                 [parts : (Listof part)])])

;; =============================================================================

(define-type RenderClass
  (Class
   (field [report-output?? Any]
          [dest-dir Any]
          [root-path Any]
          [helper-file-prefix Any]
          [refer-to-existing-files Any]
          [prefix-file Any]
          [style-file Any]
          [style-extra-files Any]
          [extra-files Any]
          [image-preferences Any]
   )
   [skip-extra-file? (-> Any Any)] ;; (-> Any Boolean)
   [traverse-block (-> Any Any Any)]
   [resolve (-> Any Any Any Any)]
   [extract-pretitle (-> Any Any)]
   [get-suffix (-> Any)] ;;[get-suffix (-> Bytes)]
   [table-of-contents (-> Any Any Any)]
   [index-manual-newlines? (-> Any)] ;;Boolean
   [render (-> Any Any Any Any)]
   [extract-version (-> Any Any)]
   [path->root-relative (-> Any Any)]
   [local-table-of-contents (-> Any Any Any Any)]
   [number-depth (-> Any Any)]
   [report-output? (-> Any)]
   [traverse-table (-> Any Any Any)]
   [report-output! (-> Any)]
   [set-external-tag-path (-> Any Any)] ;; (-> String Void)
   [deserialize-info (-> Any Any Any)] ;; (->* (Any collect-info?) (#:root-path (U Path-String #f)) Void)
   [collect-flow (-> Any Any Any)]
   [format-number (-> Any Any Any)]
   [set-external-root-url (-> Any Any)]
   [add-extra-script-file (-> Any Any)] ;; (-> Any Void)
   [set-directory-depth (-> Any Any)] ;; (-> Natural Void)
   [traverse-content (-> Any Any Any)]
   [get-dest-directory (-> Any)] ;; (->* () (Boolean) Any)
   [extract-part-style-files (-> Any Any Any Any Any Any)]
   [resolve-content (-> Any Any Any Any)]
   [render-block (-> Any Any Any Any Any)]
   [string-to-implicit-styles (-> Any Any)]
   [start-traverse (-> Any Any Any Any)]
   [sort-image-requests (-> Any Any Any)]
   [get-substitutions (-> Any)]
   [extract-date (-> Any Any)]
   [traverse-flow (-> Any Any Any)]
   [current-render-mode (-> Any)]
   [root-relative->path (-> Any Any)]
   [install-extra-files (-> Any Any)]
   [extract-authors (-> Any Any)]
   [root-relative? (-> Any Any)]
   [quiet-table-of-contents (-> Any Any Any)]
   [render-other (-> Any Any Any Any)]
   [serialize-info (-> Any Any)]
   [fresh-tag-collect-context? (-> Any Any Any)]
   [serialize-one-ht (-> Any Any Any)]
   [collect-index-element (-> Any Any Any)]
   [fresh-tag-resolve-context? (-> Any Any Any)]
   [fresh-tag-render-context? (-> Any Any Any)]
   [get-serialize-version (-> Any)]
   [resolve-flow (-> Any Any Any Any)]
   [render-nested-flow (-> Any Any Any Any Any)]
   [collect (-> Any Any Any Any Any)] ;; 4th arg optional
   [render-auxiliary-table (-> Any Any Any Any)]
   [render-itemization (-> Any Any Any Any)]
   [render-intrapara-block (-> Any Any Any Any Any Any Any)]
   [get-defined (-> Any Any)]
   [serialize-infos (-> Any Any Any Any)]
   [render-paragraph (-> Any Any Any Any)]
   [get-defineds (-> Any Any Any Any)]
   [transfer-info (-> Any Any Any)]
   [traverse-part (-> Any Any Any)]
   [traverse-paragraph (-> Any Any Any)]
   [install-file (-> Any Any Any)] ;; (->* Any (Any #:private-name? Any) Any)
   [get-external (-> Any Any)]
   [traverse-itemization (-> Any Any Any)]
   [traverse-nested-flow (-> Any Any Any)]
   [collect-part (-> Any Any Any Any Any Any)]
   [traverse-compound-paragraph (->  Any Any Any)]
   [collect-itemization (-> Any Any Any)]
   [render-table (-> Any Any Any Any Any)]
   [start-collect (-> Any Any Any Any)]
   [resolve-table (-> Any Any Any Any)]
   [render-content (-> Any Any Any Any)]
   [render-flow (-> Any Any Any Any Any)]
   [start-resolve (-> Any Any Any Any)]
   [render-compound-paragraph (-> Any Any Any Any Any)]
   [collect-part-tags (-> Any Any Any Any)]
   [collect-table (-> Any Any Any)]
   [render-part-content (-> Any Any Any)]
   [get-undefined (-> Any Any)]
   [collect-paragraph (-> Any Any Any)]
   [collect-block (-> Any Any Any)]
   [resolve-block (-> Any Any Any Any)]
   [render-one (-> Any Any Any Any)]
   [render-part (-> Any Any Any)]
   [traverse (-> Any Any Any)]
   [collect-nested-flow (-> Any Any Any)]
   [collect-compound-paragraph (-> Any Any Any)]
   [resolve-paragraph (-> Any Any Any Any)]
   [resolve-part (-> Any Any Any)]
   [collect-content (-> Any Any Any)]
   [collect-target-element (-> Any Any Any)]
   [resolve-itemization (-> Any Any Any Any)]
   [resolve-nested-flow (-> Any Any Any Any)]
   [resolve-compound-paragraph (-> Any Any Any Any)]
   [auto-extra-files? (-> Any Any)]
   [auto-extra-files-paths (-> Any Any)]
   ;; -- html-render
   ;[part-whole-page? (-> Any Any Any)]
   ))

(define-type RenderMixin
  (-> (Class #:implements RenderClass)
      (Class #:implements RenderClass)))
