(set-env!
    :source-paths #{"src/cljs"}
    :resource-paths #{"html"}
    :dependencies '[
                    [org.clojure/clojure "1.7.0"]
                    [org.clojure/core.async "0.2.374"]
                    [org.clojure/clojurescript "1.7.170"]
                    [adzerk/boot-cljs "1.7.170-3"]
                    [pandeiro/boot-http "0.7.0"]
                    [adzerk/boot-reload "0.4.2"]
                    [adzerk/boot-cljs-repl "0.3.0"]
                    [com.cemerick/piggieback "0.2.1"]
                    [weasel "0.7.0"]
                    [org.clojure/tools.nrepl "0.2.12"]
                    [tailrecursion/cljs-priority-map "1.1.0"]
                    [alandipert/storage-atom "1.2.4"]
                   ])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-reload :refer [reload]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]])

(deftask w
    []
    (comp
        (watch)
        (speak)
        (cljs)
        (target :dir #{"target"})))

(deftask dev
    []
    (comp
        (serve :dir "target")
        (watch)
        (reload)
        (cljs-repl)
        (cljs)
        (target :dir #{"target"})))

(deftask prod
    []
    (comp
        (cljs :optimizations :advanced)
        (target :dir #{"target"})))

