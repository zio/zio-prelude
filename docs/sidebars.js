const sidebars = {
  sidebar: [
    "index",
    "abstraction-diagrams",
    {
      type: "category",
      label: "Functional Abstractions",
      link: { type: "doc", id: "functionalabstractions/index" },
      collapsed: false,
      items: [
        {
          type: "category",
          label: "Concrete Types",
          link: { type: "doc", id: "functionalabstractions/concretetypes/index" },
          collapsed: false,
          items: [
            "functionalabstractions/concretetypes/associative",
            "functionalabstractions/concretetypes/commutative",
            "functionalabstractions/concretetypes/debug",
            "functionalabstractions/concretetypes/equal",
            "functionalabstractions/concretetypes/hash",
            "functionalabstractions/concretetypes/identity",
            "functionalabstractions/concretetypes/inverse",
            "functionalabstractions/concretetypes/ord"
          ]
        },
        {
          type: "category",
          label: "Parameterized Types",
          link: { type: "doc", id: "functionalabstractions/parameterizedtypes/index" },
          collapsed: false,
          items: [
            "functionalabstractions/parameterizedtypes/associativeboth",
            "functionalabstractions/parameterizedtypes/associativeeither",
            "functionalabstractions/parameterizedtypes/associativeflatten",
            "functionalabstractions/parameterizedtypes/commutativeboth",
            "functionalabstractions/parameterizedtypes/commutativeeither",
            "functionalabstractions/parameterizedtypes/contravariant",
            "functionalabstractions/parameterizedtypes/covariant",
            "functionalabstractions/parameterizedtypes/foreach",
            "functionalabstractions/parameterizedtypes/identityboth",
            "functionalabstractions/parameterizedtypes/identityeither",
            "functionalabstractions/parameterizedtypes/identityflatten",
            "functionalabstractions/parameterizedtypes/invariant",
            "functionalabstractions/parameterizedtypes/nonemptyforeach"
          ]
        },

      ]
    },
    {
      type: "category",
      label: "Functional Data Types",
      link: { type: "doc", id: "functionaldatatypes/index" },
      collapsed: false,
      items: [
        "functionaldatatypes/equivalence",
        "functionaldatatypes/nonemptylist",
        "functionaldatatypes/these",
        "functionaldatatypes/validation",
        "functionaldatatypes/zset",
        "functionaldatatypes/zvalidation"
      ]
    },
    "newtypes/index",
    "zpure/index"
  ]
};

module.exports = sidebars;
