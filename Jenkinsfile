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
    }
}
