pipeline {
    agent { label 'haskell' }
    stages {
        stage('syntax') {
            steps {
                sh 'stack exec -- hlint --git'
            }
        }
        stage('build') {
            steps {
                sh 'stack build'
            }
        }
        stage('run') {
            steps {
                sh 'stack exec -- dotaCleave'
            }
        }
        stage('build-nightly') {
            steps {
                sh 'stack --resolver nightly build'
            }
        }
        stage('run-nightly') {
            steps {
                sh 'stack exec --resolver nightly -- dotaCleave'
            }
        }
    }
}
