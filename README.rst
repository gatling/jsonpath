########
JSONPath
########

This is an implementation of JSONPath in Scala. The library follows the semantics describted by Stefan Goessner on his `blog <http://goessner.net/articles/JsonPath>`_. Although, due to the lack of standardization, it is worth reading the following documentation. 


Syntax
======

JSONPath expressions
--------------------

In a JSONPath expression, the abstract name ``$`` is used to designate to the outer level object.

JSONPath expressions can use the dot–notation ``$.store.book[0].title`` or the bracket–notation ``$['store']['book'][0]['title']``.

Based on the following JSON document : 

.. code-block:: json

    { "store": {
        "book": [ 
          { "category": "reference",
            "author": "Nigel Rees",
            "title": "Sayings of the Century",
            "price": 8.95
          },
          { "category": "fiction",
            "author": "Evelyn Waugh",
            "title": "Sword of Honour",
            "price": 12.99
          },
          { "category": "fiction",
            "author": "Herman Melville",
            "title": "Moby Dick",
            "isbn": "0-553-21311-3",
            "price": 8.99
          },
          { "category": "fiction",
            "author": "J. R. R. Tolkien",
            "title": "The Lord of the Rings",
            "isbn": "0-395-19395-8",
            "price": 22.99
          }
        ],
        "bicycle": {
          "color": "red",
          "price": 19.95
        }
      }
    }

Here is a list of supported operators : 

+----------------------+--------------------------------+---------------------------------------------+
| Operator             | Description                    | Example                                     |
+======================+================================+=============================================+
| ``$``                | the root element               | ``$``                                       |
+----------------------+--------------------------------+---------------------------------------------+
| ``@``                | the current element            | ``@``                                       |
+----------------------+--------------------------------+---------------------------------------------+
| ``.`` or ``[]``      | the child element              | ``$.bicycle or $['bicycle']``               |
+----------------------+--------------------------------+---------------------------------------------+
| ``..``               | recursive descent              | ``$..author``                               |
+----------------------+--------------------------------+---------------------------------------------+
| ``*``                | wildcard                       | ``$.store.book[*].author``                  |
+----------------------+--------------------------------+---------------------------------------------+
| ``[,]``              | collection of names or indices | ``$[2,5], $.store.book['title', 'author']`` |
+----------------------+--------------------------------+---------------------------------------------+
| ``[start:stop:end]`` | array slice                    | ``$[2:-3]``, ``$[::-1]``                    |
+----------------------+--------------------------------+---------------------------------------------+
| ``[?(expression)]``  | filter expression              | ``$.store.book[?(@.price < 10)].title``     |
+----------------------+--------------------------------+---------------------------------------------+


Filter expressions
------------------

The underlying engine for writing filter expressions currently support two different kinds of filter expression:

+-------------------+----------------------------------------------------+
| Filter            | Example                                            |
+===================+====================================================+
| Has-child filter  | ``$.store.book[?(@.isbn)].title``                  |
+-------------------+----------------------------------------------------+
| Comparison filter | ``$.store.book[?(@.category == 'fiction')].title`` |
+-------------------+----------------------------------------------------+

You can also combine them with boolean operators, eg: ``$.store.book[?(@.price < 10 && @.price >4)].title``


Implementation
==============

This implementation currently relies on `Json-Smart <http://code.google.com/p/json-smart>`_ for parsing the JSON document. The library can easily be adapted to use different JSON providers. 

``JsonPath.query("$.a", """{"a":"A","b":"B"}""")`` gives you ``Right(non-empty iterator)``. This will allow you to iterate over all possible solutions to the query. 

eg :  
``JsonPath.query("$.a", """{"a":"A","b":"B"}""").right.map(_.toVector)`` gives you ``Right(Vector("A"))``

Benchmark vs Jayway's implementation
====================================

+-----------------------------------------------------------+---------+--------+
| twitterQueries                                            | Gatling | Jayway |
+===========================================================+=========+========+
| $.results[:3].from_user                                   | 0.162   | 0.05   |
+-----------------------------------------------------------+---------+--------+
| $.completed_in                                            | 0.07    | 0.022  |
+-----------------------------------------------------------+---------+--------+
| $.results[?(@.from_user == 'origichara_bot')]             | 0.307   | 0.185  |
+-----------------------------------------------------------+---------+--------+

+-----------------------------------------------------------+---------+--------+
| precompiled twitterQueries                                | Gatling | Jayway |
+===========================================================+=========+========+
| $.results[:3].from_user                                   | 0.021   | 0.019  |
+-----------------------------------------------------------+---------+--------+
| $.completed_in                                            | 0.006   | 0.006  |
+-----------------------------------------------------------+---------+--------+
| $.results[?(@.from_user == 'origichara_bot')]             | 0.094   | 0.159  |
+-----------------------------------------------------------+---------+--------+

Conclusion: not as fast as Jayway on compilation (to be expected due to Parser Combinators).
However, that doesn't matter once compiled and cached (like in Gatling). Graph traversal can be even better on some complex use cases (liek the last one).

Licence
=======

This library is licensed under the `Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0>`_

