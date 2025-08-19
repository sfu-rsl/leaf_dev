use bincode::Encode;
use serde::{Serialize, Serializer};

type GenericError = Box<dyn core::error::Error>;

type Result = core::result::Result<(), GenericError>;

// Limiting the interface to only a single type so we can make it dyn compatible.
pub(crate) trait TypeSerializer<T> {
    fn serialize(&mut self, value: &T) -> Result;
}

#[allow(coherence_leak_check)]
impl<T> TypeSerializer<T> for Box<dyn TypeSerializer<T> + '_> {
    fn serialize(&mut self, value: &T) -> Result {
        self.as_mut().serialize(value)
    }
}

impl<T: Serialize, S> TypeSerializer<T> for S
where
    for<'a> &'a mut S: Serializer,
    for<'a> <&'a mut S as Serializer>::Error: 'static,
{
    fn serialize(&mut self, value: &T) -> Result {
        value
            .serialize(self)
            .map(|_| ())
            .map_err(|e: <&mut S as Serializer>::Error| e.into())
    }
}

pub(crate) struct BincodeAdapter<W>(pub W);

impl<T: Encode, W: std::io::Write> TypeSerializer<T> for BincodeAdapter<W> {
    fn serialize(&mut self, value: &T) -> Result {
        use bincode::config::*;
        bincode::encode_into_std_write::<_, Configuration<LittleEndian, Varint, NoLimit>, _>(
            value,
            &mut self.0,
            Default::default(),
        )
        .map(|_| ())
        .map_err(|e| e.into())
    }
}
