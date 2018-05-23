def stack(String command, String resolver = 'nightly') {
    sh "stack --resolver ${resolver} ${command}"
}

def resolvers = ['nightly', 'lts-11.10', 'lts-9.21']

def setupStepParallel = resolvers.collectEntries {
    ["setup-${it}" : setupStep(it)]
}

def setupStep(r) {
    return {
        node {
            stack("setup", "${r}")
        }
    }
}

def setupStepExec = resolvers.collectEntries {
    ["exec-${it}" : execStep(it)]
}

def execStep(r) {
    return {
        node {
            stack("setup", "${r}")
        }
    }
}

node('bootstrap') {
    stage("checkout") {
        checkout scm
    }
    stage("printing") {
        echo env.BUILD_ID
        echo env.CHANGE_ID
        echo env
    }
    stage("bootstraps") {
        parallel setupStepParallel
    }
    stage("builds") {
        for (r in resolvers) {
            stack("build", "${r}")
        }
    }
    stage("run") {
        parallel setupStepExec
    }
}
