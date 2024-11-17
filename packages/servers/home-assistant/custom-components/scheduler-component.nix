{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
}:

buildHomeAssistantComponent rec {
  owner = "nielsfaber";
  domain = "scheduler";
  version = "3.3.8";

  src = fetchFromGitHub {
    inherit owner;
    repo = "scheduler-component";
    rev = "v${version}";
    hash = "sha256-QN7rkNuj9IBbV2ths7ZdL/EkXFJUpjNbgJNUnAHjLBA=";
  };

  meta = with lib; {
    description = "Custom component for HA that enables the creation of scheduler entities";
    homepage = "https://github.com/nielsfaber/scheduler-component";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ];
    mainProgram = "scheduler-component";
    platforms = platforms.all;
  };
}
