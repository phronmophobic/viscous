# Viscous

A visual data inspector/explorer that runs in constant space, time, and screen space.

Built with [membrane](https://github.com/phronmophobic/membrane).

### Overview

![Overview](inspector.gif?raw=true)

### Resizing
![Resizing](inspector-resize.gif?raw=true)

### Paging
![Paging](paging.gif?raw=true)

### Navigating
![Navigating](navigation.gif?raw=true)

## Web Demo

Try it! https://phronmophobic.github.io/viscous/

## Dependency

#### deps.edn
```clojure
com.phronemophobic/viscous {:mvn/version "1.3.5"}
```

#### lein
```clojure
[com.phronemophobic/viscous "1.3.5"]
```

## Programmatic Usage

```clojure
(require '[com.phronemophobic.viscous :as viscous])

(def my-data {:a {:b 42}})

;; open inspector window
(viscous/inspect my-data)

```

## Cli usage

Create an alias for viscous

```clojure

{
 :aliases {
```
```clojure
  :viscous
  {:replace-deps {com.phronemophobic/viscous {:mvn/version "1.3.5"}
                  org.clojure/data.json {:mvn/version "2.4.0"}}
   :exec-fn com.phronemophobic.viscous.cli/main}
```
```clojure
           }
}
```

Read edn from stdin:
```sh
cat data.edn | clojure -X:viscous :file -
```

Read edn from filename:
```sh
clojure -X:viscous :file '"data.edn"'
```

Read json from stdin:
```sh
cat data.json | clojure -X:viscous :json-file -
```

Read json from filename:
```sh
clojure -X:viscous :json-file '"data.edn"'
```

## Status

Viscous is in beta. Behavior is subject to change. Feedback is welcome.

## Related

I'm generally interested in finding better ways to represent and explore medium sized heterogenous data. If you like viscous, you may also be interested in [treemap-clj](https://github.com/phronmophobic/treemap-clj).

## License

Copyright © 2025 Adrian Smith

Distributed under the Eclipse Public License version 1.0.
