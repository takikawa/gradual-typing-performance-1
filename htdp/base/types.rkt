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
 [#:opaque ResolveInfo resolve-info?]
 [#:opaque CollectInfo collect-info?]
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
   [traverse (-> (Listof part) (Listof Path-String) Any)]
   [collect (->* [(Listof part) (Listof Path-String) Any]
                 [(-> Tag CollectInfo Any)]
                 CollectInfo)]
   [resolve (-> (Listof part) (Listof Path-String) CollectInfo ResolveInfo)]
   ;; Note: docs for render<%> are wrong, it doesn't necessarily return void
   [render (-> (Listof part) (Listof Path-String) ResolveInfo Any)]
   [serialize-info (-> ResolveInfo Any)]
   [serialize-infos (-> ResolveInfo Integer part (Listof Any))]
   [deserialize-info (->* (Any CollectInfo) (#:root (U Path-String #f)) Void)]
   [get-defined (-> CollectInfo (Listof Tag))]
   [get-defineds (-> CollectInfo Integer part (Listof (Listof Tag)))]
   [get-external (-> ResolveInfo (Listof Tag))]
   [get-undefined (-> ResolveInfo (Listof Tag))]))

(define-type RenderMixin
  (All (r #:row)
    (-> (Class #:row-var r #:implements RenderClass)
        (Class #:row-var r #:implements RenderClass))))
