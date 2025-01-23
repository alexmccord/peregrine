use thiserror::Error;

#[derive(Debug)]
pub struct Manifest {}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ManifestError {}

impl Manifest {
    pub fn parse(str: String) -> Result<Manifest, ManifestError> {
        Ok(Manifest {})
    }
}

impl TryFrom<String> for Manifest {
    type Error = ManifestError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Manifest::parse(value)
    }
}
