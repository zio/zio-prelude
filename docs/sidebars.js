const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Prelude",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        {
          type: "category",
          label: "Functional Abstractions",
          link: { type: "doc", id: "functional-abstractions/index" },
          collapsed: false,
          items: [
            "functional-abstractions/abstraction-diagrams",
            {
              type: "category",
              label: "Concrete Types",
              link: { type: "doc", id: "functional-abstractions/concrete-types/index" },
              collapsed: false,
              items: [
                "functional-abstractions/concrete-types/associative",
                "functional-abstractions/concrete-types/commutative",
                "functional-abstractions/concrete-types/debug",
                "functional-abstractions/concrete-types/equal",
                "functional-abstractions/concrete-types/hash",
                "functional-abstractions/concrete-types/idempotent",
                "functional-abstractions/concrete-types/identity",
                "functional-abstractions/concrete-types/inverse",
                "functional-abstractions/concrete-types/ord",
                "functional-abstractions/concrete-types/partialequivalence",
                "functional-abstractions/concrete-types/partialinverse",
                "functional-abstractions/concrete-types/partialord"
              ]
            },
            {
              type: "category",
              label: "Parameterized Types",
              link: { type: "doc", id: "functional-abstractions/parameterized-types/index" },
              collapsed: false,
              items: [
                "functional-abstractions/parameterized-types/associativeboth",
                "functional-abstractions/parameterized-types/associativecompose",
                "functional-abstractions/parameterized-types/associativeeither",
                "functional-abstractions/parameterized-types/associativeflatten",
                "functional-abstractions/parameterized-types/bicovariant",
                "functional-abstractions/parameterized-types/commutativeboth",
                "functional-abstractions/parameterized-types/commutativeeither",
                "functional-abstractions/parameterized-types/contravariant",
                "functional-abstractions/parameterized-types/covariant",
                "functional-abstractions/parameterized-types/derive",
                "functional-abstractions/parameterized-types/divariant",
                "functional-abstractions/parameterized-types/foreach",
                "functional-abstractions/parameterized-types/identityboth",
                "functional-abstractions/parameterized-types/identitycompose",
                "functional-abstractions/parameterized-types/identityeither",
                "functional-abstractions/parameterized-types/identityflatten",
                "functional-abstractions/parameterized-types/invariant",
                "functional-abstractions/parameterized-types/nonemptyforeach",
                "functional-abstractions/parameterized-types/zivariant"
              ]
            },
          ]
        },
        {
          type: "category",
          label: "Functional Data Types",
          link: { type: "doc", id: "functional-data-types/index" },
          collapsed: false,
          items: [
            "functional-data-types/equivalence",
            "functional-data-types/id-and-const",
            "functional-data-types/nonemptylist",
            "functional-data-types/nonemptymap",
            "functional-data-types/nonemptyset",
            "functional-data-types/nonemptysorted",
            "functional-data-types/optional",
            "functional-data-types/parseq",
            "functional-data-types/recursive",
            "functional-data-types/these",
            "functional-data-types/validation",
            "functional-data-types/znonemptyset",
            "functional-data-types/zset",
            "functional-data-types/zvalidation"
          ]
        },
        {
          type: "category",
          label: "New Types",
          link: { type: "doc", id: "newtypes/index" },
          collapsed: false,
          items: [
            "newtypes/constructing",
            "newtypes/operators-and-instances",
            "newtypes/subtypes",
            "newtypes/smart-newtypes",
            "newtypes/predefined-newtypes"
          ]
        },
        {
          type: "category",
          label: "ZPure",
          link: { type: "doc", id: "zpure/index" },
          collapsed: false,
          items: [
            "zpure/basics",
            "zpure/errors-and-context",
            "zpure/state-and-logging",
            "zpure/running-and-converting"
          ]
        },
        "resources"
      ]
    }
  ]
};

module.exports = sidebars;
