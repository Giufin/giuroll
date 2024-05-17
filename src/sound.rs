use std::collections::{HashMap, HashSet};

use crate::{force_sound_skip, println, SOKU_FRAMECOUNT};

pub struct RollbackSoundManager {
    sounds_that_did_happen: HashMap<usize, Vec<usize>>,
    sounds_that_maybe_happened: HashMap<usize, Vec<usize>>,
    pub current_rollback: Option<usize>,
}

impl RollbackSoundManager {
    pub fn new() -> Self {
        Self {
            sounds_that_did_happen: HashMap::new(),
            sounds_that_maybe_happened: HashMap::new(),
            current_rollback: None,
        }
    }

    pub fn insert_sound(&mut self, frame: usize, sound: usize) -> bool {
        //if let Some(x) = self.sounds_that_maybe_happened.get_mut(&frame) {
        //    let index = x.iter().position(|x| *x == sound);
        //    if let Some(idx) = index {
        //        //x.remove(idx);
        //        return false;
        //    }
        //}

        let s = self
            .sounds_that_did_happen
            .entry(frame)
            .or_insert_with(|| Vec::new());
        s.push(sound);

        self.sounds_that_maybe_happened
            .values()
            .map(|x| x.iter())
            .flatten()
            .find(|x| **x == sound)
            .is_none()
    }

    pub fn pop_sounds_since(&mut self, from: usize, to: usize) {
        //println!("popping sounds from {from} to {to}");

        match std::mem::replace(&mut self.current_rollback, Some(from)) {
            Some(x) => println!("should have crashed sound 45"),
            None => (),
        };

        for a in from..=to {
            if let Some(old_sounds) = self.sounds_that_did_happen.remove(&a) {
                self.sounds_that_maybe_happened.insert(a, old_sounds);
            }
        }
    }

    pub fn delete_non_matched(&mut self) {
        let old_sounds = std::mem::replace(&mut self.sounds_that_maybe_happened, HashMap::new());
        let fc = unsafe { *SOKU_FRAMECOUNT };
        let old = match self.current_rollback.take() {
            Some(x) => x,
            None => {
                println!("should have crashed here, sound 65");
                return;
                //panic!();
            }
        };

        let new_sounds: HashSet<usize> = (old..=fc)
            .flat_map(|x| self.sounds_that_did_happen.remove(&x))
            .map(|x| x.into_iter())
            .flatten()
            .collect();

        for (frame, sound) in old_sounds
            .into_iter()
            .map(|(frame, sounds)| sounds.into_iter().map(move |x| (frame, x)))
            .flatten()
        {
            //to do: not only delete sounds, but also restart them/shift them

            if !new_sounds.contains(&sound) {
                let played_recently = (old.saturating_sub(4)..old)
                    .filter_map(|x| self.sounds_that_did_happen.get(&x))
                    .map(|x| x.iter())
                    .flatten()
                    .find(|x| **x == sound)
                    .is_some();

                if !played_recently {
                    force_sound_skip(sound);

                    /*println!(
                        "sound {}, from frame {} deleted at frame {}",
                        sound,
                        frame,
                        unsafe { *SOKU_FRAMECOUNT },
                    ); */
                } else {
                    //println!("sound {} would have been skipped but wasnt", sound)
                }
            } else {
                self.sounds_that_did_happen
                    .entry(frame)
                    .or_insert_with(|| Vec::new())
                    .push(sound)
                //self.insert_sound(*frame, *sound);
                //println!("sound retained");
            }
        }
    }
}
