#r "System.Xml.Linq"

open System
open System.IO
open System.Xml.Linq

module Helpers =
    module Option =
        let ofString (s : string) =
            if String.IsNullOrEmpty(s) then None else Some(s)

    module Array =
        let groupBy2 (keyProjection : 'a -> 'Key) (valueProjection : 'a -> 'Value) =
            Array.groupBy keyProjection
            >> Array.map (fun (key, items) -> (key, Array.map valueProjection items))

    module String =
        let toLowerInvariant (s : string) = s.ToLowerInvariant()
        let split (sep : char) (s : string) = s.Split(sep)
        let lastPart (n : int) (s : string) = s.Substring(n)

    module Xml =
        
        let inline xn (s : string) = XName.Get(s)

        let load (uri : string) = XElement.Load(uri)

        let save (fileName : string) (e : XElement) = 
            let settings = System.Xml.XmlWriterSettings(OmitXmlDeclaration = true, Indent=true)
            use writer = System.Xml.XmlWriter.Create(fileName, settings)
            e.Save(writer)

        let elements (name : string) (e : XElement) = e.Elements(xn name)

        let setAttributeValue (name : string) (newValue : string) (e : XElement) =
            e.SetAttributeValue(xn name, newValue)

        let optionalAttribute (name : string) (e : XElement) = 
            let attr = e.Attribute(xn name)
            if isNull attr then None
            else Option.ofString attr.Value

        let requiredAttribute (name : string) (e : XElement) = 
            match optionalAttribute name e with
            | Some v -> v
            | None -> 
                let elName = e.Name.ToString()
                let msg = sprintf "Required attribute '%s' was not present in '%s' element" name elName
                raise (System.ArgumentException(msg, "name"))

        let collectElements<'a> (selector : XElement -> XElement seq) (mapper : XElement -> 'a) (el : XElement) =
            el |> selector
               |> Seq.map mapper


module Models =
    open Helpers


    type PackageVersion =
        {   Major : int
            Minor : int
            Patch : int 
            Extra : string }
        override this.ToString() = 
            let s = sprintf "%d.%d.%d" this.Major this.Minor this.Patch
            if this.Extra = "" then s else s + "-" + this.Extra

 
    type PackageRef = 
        {   Name    : String
            Version : PackageVersion   }


    type ProjectInfo =
        {   ProjectFilePath : String
            PackageRefs     : PackageRef[]   }


    module PackageRef =
        let keyOf packageRef = (String.toLowerInvariant packageRef.Name, packageRef.Version)        

        let parseVersion versionStr =
            if String.IsNullOrWhiteSpace(versionStr) then nullArg "versionStr"

            let parts = String.split '.' versionStr
            let major = int parts.[0]
            let mutable minor = 0
            let mutable patch = 0
            let mutable extra = ""
            if parts.Length > 1 then minor <- int parts.[1]
            if parts.Length > 2 then
                let patchPart = parts.[2]
                let dashIdx = patchPart.IndexOf("-")
                if dashIdx = -1 then patch <- int patchPart
                else patch <- int (patchPart.Substring(0,dashIdx))
                     extra <- patchPart.Substring(dashIdx+1)
            {   Major = major; Minor = minor; Patch = patch; Extra = extra  }

        let fromXml (e : XElement) =
            let name = e |> Xml.requiredAttribute "Include"
            let version = e |> Xml.requiredAttribute "Version" |> parseVersion
            {   PackageRef.Name = name
                Version = version        }

        // Compares two package versions
        //
        // This function implements the following behavior:
        // 5.10.15 < 10.0.0 => true
        // 1.0.0-beta2 > 1.0.0-alpha6 => true
        // 1.0.0-rc1 > 1.0.0-beta4 => true
        // 1.0.0 > 1.0.0-rc2 => true
        let compareVs lhs rhs =
            let majorDiff = lhs.Major - rhs.Major
            if majorDiff <> 0 then majorDiff 
            else let minorDiff = lhs.Minor - rhs.Minor
                 if minorDiff <> 0 then minorDiff
                 else let patchDiff = lhs.Patch - rhs.Patch
                      if patchDiff <> 0 then patchDiff
                      else // note the switch of rhs and lhs
                           let extraDiff = String.CompareOrdinal(rhs.Extra, lhs.Extra) 
                           if extraDiff = 0 then 0
                           else if rhs.Extra = "" then -1 
                                elif lhs.Extra = "" then 1
                                else extraDiff


    module ProjectInfo =
        let collectPackageRefs =
    
            Xml.elements "ItemGroup" >> Seq.collect (Xml.elements "PackageReference")  

        let fromXml (projectFilePath : string) =
            let packageRefs = Xml.load projectFilePath 
                              |> Xml.collectElements collectPackageRefs PackageRef.fromXml
                              |> Seq.toArray
            {   ProjectFilePath = projectFilePath 
                PackageRefs = packageRefs  }


module Action =
    open Models
    open Helpers

    type Action =
        | Help 
        | Consolidate
        | Report
        | Upgrade of packageId:string * version:Models.PackageVersion


    let private printUsage () =
        let printAligned (s : string) =
            s.Split('\n')
            |> Array.map (fun line -> line.Substring(line.IndexOf("|")+1))
            |> Array.iter (printfn "%s")

        """|
           |USAGE: dotnet updatepkg ACTION [options]
           |
           |where ACTION is one of
           |   help                    - shows this info
           |   consolidate             - ensures all projects in solution runs same version of packages
           |   report                  - prints out list of currently used package versions
           |   upgrade PACKAGE VERSION - upgrades a single package to a specified version across all projects
           |                             in solution
           |"""
        |> printAligned

    let private stringToAction (options : string array) = 
        let actionName = options.[0]
        match String.toLowerInvariant actionName with
        | "help" -> Ok(Help)
        | "consolidate" -> Ok(Consolidate)
        | "report" -> Ok(Report)
        | "upgrade" -> 
            if options.Length < 3 then Error("Missing options for upgrade action")
            else let packageId = options.[1]
                 try
                     let requestedVersion = options.[2] |> PackageRef.parseVersion
                     Ok(Upgrade(packageId, requestedVersion))
                 with :? System.FormatException -> Error("version option not in correct format")
        | _ -> Error(sprintf "Unknown action '%s'" actionName)

    let private parse options = 
        if Array.length options < 2 then Ok(Help)
        else stringToAction (Array.tail options)

    let private allPackages projectInfos =
        projectInfos
        |> Array.collect (fun pi -> pi.PackageRefs)
        |> Array.distinctBy PackageRef.keyOf
        |> Array.sortWith (fun pr1 pr2 -> PackageRef.compareVs pr1.Version pr2.Version)
        |> Array.rev  
        |> Array.groupBy (fun p -> p.Name.ToLowerInvariant())
        |> Array.sortBy fst
    
    let private loadProjectInfos rootFolder =
        Directory.EnumerateFiles(rootFolder, "*.*proj", SearchOption.AllDirectories) 
        |> Seq.map ProjectInfo.fromXml
        |> Seq.toArray

    let private makeProjectFinder projectInfos =
        let packageProjectsLookup = 
            [| for pi in projectInfos do 
                for pr in pi.PackageRefs -> (PackageRef.keyOf pr, pi.ProjectFilePath) |]
            |> Array.groupBy2 fst snd
            |> Map.ofArray   
        (fun packageRef ->
            match packageProjectsLookup.TryFind (PackageRef.keyOf packageRef) with
            | None -> failwithf "Package %s %O is not used by any projects." packageRef.Name packageRef.Version
            | Some projectPaths -> projectPaths )

    let private findPackageById packageId packages = 
        let isEq (pn, _) = String.Equals(packageId, pn, StringComparison.InvariantCultureIgnoreCase)
        match Array.tryFind isEq packages with
        | None -> eprintfn "ERROR: Package %s not used by any projects" packageId; exit(1)
        | Some p -> p

    let private upgradeProject (projectPath : string) (packageName : string) (destVersion : PackageVersion) =
        let doc = Xml.load projectPath
        doc |> Xml.collectElements ProjectInfo.collectPackageRefs id
            |> Seq.filter (fun e -> (Xml.requiredAttribute "Include" e |> String.toLowerInvariant) = packageName)
            |> Seq.iter (Xml.setAttributeValue "Version" (sprintf "%O" destVersion))
        Xml.save projectPath doc
        
    let report rootFolder =
        printfn "Starting report ..."
        let projectInfos = loadProjectInfos rootFolder
        let findProjectsThatUse = makeProjectFinder projectInfos    

        for (packageName, packageRefs) in allPackages projectInfos do
            printfn "\n%s" packageName
            for packageRef in packageRefs do
                printfn "  %O" packageRef.Version
                findProjectsThatUse packageRef
                |> Array.map (String.lastPart (rootFolder.Length+1))
                |> Array.iter (printfn "      %s")

    let consolidate rootFolder =
        printfn "Starting consolidation ..."
        let projectInfos = loadProjectInfos rootFolder
        let packages = allPackages projectInfos
        let findProjectsThatUse = makeProjectFinder projectInfos
        let bestVs = 
            packages
            |> Array.map (fun (_, refs) -> Array.head refs |> PackageRef.keyOf)
            |> Map.ofArray
        let projectsInNeedOfUpgrading = 
            packages
            |> Array.filter (fun (_, refs) -> refs.Length > 1)
            |> Array.collect (fun (_, refs) -> Array.skip 1 refs)
            |> Array.collect (fun pRef -> findProjectsThatUse pRef |> Array.map (fun proj -> proj, pRef))
            |> Array.groupBy2 fst snd
        let mutable upgradedAtLeastOne = false
        for projName, packagesToUpgrade in projectsInNeedOfUpgrading do
            printfn "In %s:" (projName |> String.lastPart (rootFolder.Length+1))
            for pRef in packagesToUpgrade do
                let pName = pRef.Name |> String.toLowerInvariant
                let destVs = bestVs.TryFind pName |> Option.get
                printfn "    Upgrade %s from %O to %O" pName pRef.Version destVs
                upgradeProject projName pName destVs
                upgradedAtLeastOne <- true

        if upgradedAtLeastOne then printfn "\nRemember to run dotnet restore"
        else printfn "\nNo packages upgraded"

    let upgrade rootFolder packageId (requestedVersion : PackageVersion) =     
        printfn "Starting upgrade of package %s to version %O ..." packageId requestedVersion
        let projectInfos = loadProjectInfos rootFolder
        let findProjectsThatUse = makeProjectFinder projectInfos
        let packageName, pkgRefs = projectInfos |> allPackages |> findPackageById packageId
        let mutable upgradedAtLeastOne = false
        for pRef in pkgRefs |> Array.filter (fun pr -> pr.Version <> requestedVersion) do
            printfn "\nUpgrading %s from %O ..." packageName pRef.Version
            for projPath in findProjectsThatUse pRef do
                printfn "  ... in %s" (String.lastPart (rootFolder.Length+1) projPath)
                upgradeProject projPath packageName requestedVersion
                upgradedAtLeastOne <- true

        if upgradedAtLeastOne then printfn "\nRemember to run dotnet restore"
        else printfn "\nNo packages upgraded"

    let parseAndRun rootFolder options =
        match parse options with
        | Error msg -> printfn "ERROR: %s" msg
                       printUsage()
                       exit 1
        | Ok action -> match action with
                       | Help -> printUsage(); exit 0
                       | Consolidate -> consolidate rootFolder
                       | Report -> report rootFolder
                       | Upgrade (project, version) -> upgrade rootFolder project version        

fsi.CommandLineArgs |> Action.parseAndRun System.Environment.CurrentDirectory
