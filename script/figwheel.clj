(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra])

(ra/start-figwheel!
  {:figwheel-options {:css-dirs ["resources/public/css"]}
   :build-ids ["dev" "devcards"]
   :all-builds [{:id "dev"
                 :figwheel true
                 :source-paths ["src"]
                 :compiler {:main 'robozzle.core
                            :asset-path "js/compiled/out"
                            :output-to "resources/public/js/compiled/robozzle.js"
                            :output-dir "resources/public/js/compiled"
                            :verbose true
                            :source-map-timestamp true}}
                {:id "devcards"
                 :source-paths ["src"]
                 :figwheel {:devcards true}
                 :compiler {:main 'robozzle.core
                            :asset-path "js/compiled/devcards_out"
                            :output-to  "resources/public/js/compiled/robozzle_devcards.js"
                            :output-dir "resources/public/js/compiled/devcards_out"
                            :source-map-timestamp true}}]})

(ra/cljs-repl)
