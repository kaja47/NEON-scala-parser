NEON Scala parser
=================

Parser for Neon file format (ne-on.org) written in Scala with magic of parser combinators.

(not yet completed, few minor features are still missing)

usage
-----

    val str = """
    
    language: neon
    features:
      - like yaml
      - like json
      - { missing: specification }
    
    usage:
      - nette framework
      - nothing else
    
    coolnes level: pretty cool
    
    """
    
    val ast = Neon(str) // and that's it
