# aml2uml

Files to convert a CIM (Cloud Information Model) schema.yaml to a set of UML diagrams.  Uses the amf/aml libraries to parse
and generates files in the PlantUML dialect.  These files can then be passed through PlantUML to generate SVG or PNG files.

To execute the code, call:

   sbt run <path to a CIM entity group>

The program will scan for all the schema files under that
point and generate a diagram covering all the schemas found.  The output file will be "diagram.uml" in the initial directory.

To pass through PlantUML, download the PlantUML jar from plantuml.com/download.  There a number of choices with various 
licences.  I've used the MIT license version with now problems.  Invoke the jar as:

   java -jar /path to jar/plantuml.jar -tsvg <path to diagram.uml file>

It will output the svg file in the same directory as diagram.svg.

This code is extremely ALPHA and was slammed together for this use case.
