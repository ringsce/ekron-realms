pipeline {
  agent any

  stages {
    stage('Checkout') {
      steps {
        git 'https://github.com/your-org/ekron-realms.git'
      }
    }

    stage('Build with Docker') {
      steps {
        sh 'docker build -t ekron-builder .'
      }
    }

    stage('Extract Binaries') {
      steps {
        sh 'docker run --rm -v $PWD/output:/output ekron-builder cp -r /app/output /output'
      }
    }
  }

  post {
    success {
      archiveArtifacts artifacts: 'output/**/*', fingerprint: true
    }
  }
}

