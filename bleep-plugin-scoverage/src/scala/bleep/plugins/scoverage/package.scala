package bleep.plugins

import bleep.{PathOps, ProjectPaths}

package object scoverage {
  def coverageDataDir(projectPaths: ProjectPaths) = projectPaths.targetDir / "scoverage-data"
}
