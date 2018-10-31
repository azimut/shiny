All things here are based or took from cbaggers's "Pushing Pixels" streams about how to use CEPL. For doing 2d stuff you don't need this much. But when you deal with cameras and pipelines and different objects you want to have a template.

         ;; Specular
         ;; (vec-to-cam (- cam-pos frag-pos))
         ;; (dir-to-cam (normalize vec-to-cam))
         ;; (reflection (normalize (reflect (- dir-to-light)
         ;;                                 frag-norm)))
         ;; (spec-foo .5)
         ;; (spec (* spec-foo (pow (saturate
         ;;                         (dot dir-to-cam reflection))
         ;;                        32)))

