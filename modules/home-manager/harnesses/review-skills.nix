# walkthrough, walkthrough-pr and prose-review: walkthrough and prose-review
# reach skill-lib/review-ui through a `lib` symlink, which this replaces with
# the real files. walkthrough-pr imports walkthrough's deck.py from beside it,
# so the three share one store path.
{ lib, runCommandLocal }:
let
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./skill-lib/review-ui
      ./skills/prose-review
      ./skills/walkthrough
      ./skills/walkthrough-pr
    ];
  };
in
runCommandLocal "review-skills" { } "cp -rL ${src}/skills $out"
