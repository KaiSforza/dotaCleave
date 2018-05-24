def stack(String command, String resolver = 'nightly') {
    sh "stack --allow-different-user --resolver ${resolver} ${command}"
}

def resolvers = ["nightly", "lts-11.10", "lts-9.21"]

def ourJobs = resolvers.collectEntries {
    [it: [ version: it
         , setup: "setup-${it}"
         , build: "build-${it}"
         , exec: "exec-${it}"
         ]
    ]
}

def stackExec = "exec -- dotaCleave"

// def setupStepParallel = resolvers.collectEntries {
//     ["setup-${it}" : setupStep(it)]
// }
//
// def setupStep(r) {
//     return {
//         node {
//             stack("setup", "${r}")
//         }
//     }
// }
//
// def setupStepExec = resolvers.collectEntries {
//     ["exec-${it}" : execStep(it)]
// }
//
// def execStep(r) {
//     return {
//         node {
//             stack("setup", "${r}")
//         }
//     }
// }


pipeline {
    agent { docker { image "fpco/stack-build"
                     args "-u root"
    } }
    stages {
        stage("printing") {
            steps {
                echo env.BUILD_ID
                echo env.CHANGE_ID
            }
        }
        stage("setup") {
            parallel {
                stage(ourJobs["nightly"].setup) {
                    steps { stack("setup", ourJobs["nightly"].version) }
                }
                stage(ourJobs["lts-11.10"].setup) {
                    steps { stack("setup", ourJobs["lts-11.10"].version) }
                }
                stage(ourJobs["lts-9.21"].setup) {
                    steps { stack("setup", ourJobs["lts-9.21"].version) }
                }
            }
        }
        stage("build-nightly") {
            steps { stack("build", resolvers[0]) }
        }
        stage("build-lts-11.10") {
            steps { stack("build", resolvers[1]) }
        }
        stage("build-lts-9.21") {
            steps { stack("build", resolvers[2]) }
        }
        stage("exec") {
            parallel {
                stage("exec-nightly") {
                    steps { stack(stackExec, resolvers[0]) }
                }
                stage("exec-lts-11.10") {
                    steps { stack(stackExec, resolvers[1]) }
                }
                stage("exec-lts-9.21") {
                    steps { stack(stackExec, resolvers[2]) }
                }
            }
        }
    }
}

            // steps {
            //     script {
            //         stage("bootstraps") {
            //             parallel setupStepParallel
            //         }
            //         // stage("builds") {
            //         //     for (r in resolvers) {
            //         //         stack("build", "${r}")
            //         //     }
            //         // }
            //         stage("run") {
            //             parallel setupStepExec
            //         }
            //     }
            // }
//         }
//     }
// }
