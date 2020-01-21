# ProjectDefFilter

Takes all Projects defined in the Metadata YAML under the key "projects" with the format:
     
     "projects" => [Project]
     String (Project Key) =>
        "language" => String
        "root" => String (Path)
        "srcRoots => [String (Path)] (optional)

and adds them as RawBlocks with the content of:
     
     <div
        class="project-def"
        data-key=<project key>
        data-language=<language>
        data-root=<root>
        data-srcRoots="<srcRoot1>,<srcRoot2>, ..."
    ></div>

to the beginning of the resulting document.

Filter is intended to be used with an HTML producing
output format only!

## building
build tested with stack
