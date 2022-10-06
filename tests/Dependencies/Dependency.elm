module Dependencies.Dependency exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type exposing (Type(..))
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create "fake/dependency"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Dependency" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "fake/dependency"
        , summary = "A fake dependency."
        , deps =
            [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.0" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Dependency"
      , comment = """A fake dependency
"""
      , aliases = []
      , unions =
            [ { name = "Wrapped"
              , comment = "A wrapped type"
              , args = []
              , tags = [ ( "Wrapped", [ Type "Int" [] ] ) ]
              }
            , { name = "Msg"
              , comment = "A non-wrapped type"
              , args = []
              , tags = [ ( "ThingClicked", [] ) ]
              }
            ]
      , binops = []
      , values = []
      }
    ]


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeModuleName moduleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeConstraint constraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
