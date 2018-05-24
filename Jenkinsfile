def stack(String command, String resolver = 'nightly') {
    sh "stack --allow-different-user --resolver ${resolver} ${command}"
}

def resolvers = ['nightly', 'lts-11.10', 'lts-9.21']

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
    agent { docker { image 'fpco/stack-build'
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
                stage("setup-nightly") {
                    steps {
                        stack("setup", "nightly")
                    }
                }
                stage("setup-lts-11.10") {
                    steps {
                        stack("setup", "lts-11.10")
                    }
                }
                stage("setup-lts-9.21") {
                    steps {
                        stack("setup", "lts-9.21")
                    }
                }
            }
        }
        stage("build") {
            stage("build-nightly") {
                steps {
                    stack("build", "nightly")
                }
            }
            stage("build-lts-11.10") {
                steps {
                    stack("build", "lts-11.10")
                }
            }
            stage("build-lts-9.21") {
                steps {
                    stack("build", "lts-9.21")
                }
            }
        }
        stage("exec") {
            parallel {
                stage("exec-nightly") {
                    steps {
                        stack("exec", "nightly")
                    }
                }
                stage("exec-lts-11.10") {
                    steps {
                        stack("exec", "lts-11.10")
                    }
                }
                stage("exec-lts-9.21") {
                    steps {
                        stack("exec", "lts-9.21")
                    }
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
