# walkthrough and walkthrough-pr: walkthrough reaches skill-lib/review-ui
# through a `lib` symlink, which this replaces with the real files.
# walkthrough-pr imports walkthrough's deck.py from beside it, so the two
# share one store path.
{ lib, runCommandLocal }:
let
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./skill-lib/review-ui
      ./skills/walkthrough
      ./skills/walkthrough-pr
    ];
  };
in
runCommandLocal "review-skills" { } "cp -rL ${src}/skills $out"
