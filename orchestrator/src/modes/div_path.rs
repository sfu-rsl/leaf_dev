use std::{
    collections::{HashMap, HashSet},
    fs::{self, OpenOptions},
    path::{Path, PathBuf},
    process::{self, Command, Stdio},
};

use sha2::{Digest, Sha256};

use common::{log_debug, log_info};

pub(crate) struct Options {
    pub program: PathBuf,
    pub child_env: HashMap<String, String>,
    pub current_inputs_dir: PathBuf,
    pub past_inputs_dir: PathBuf,
    pub next_inputs_dir: PathBuf,
}

pub(crate) fn run_loop(options: Options, target: impl Fn(&process::Output) -> bool) {
    let mut past_inputs = collect_past_inputs(&options.past_inputs_dir);
    let mut current_inputs = HashMap::new();
    fs::create_dir_all(&options.current_inputs_dir).unwrap_or_else(|err| {
        panic!(
            "Could not create current inputs directory {}: {}",
            options.current_inputs_dir.display(),
            err
        )
    });
    loop {
        current_inputs = grab_new_next_inputs(
            &options.next_inputs_dir,
            &options.current_inputs_dir,
            &past_inputs,
            current_inputs,
        );

        let Some(input) = current_inputs
            .keys()
            .into_iter()
            .next()
            .cloned()
            .map(|k| current_inputs.remove_entry(&k).unwrap())
        else {
            break;
        };

        let output = execute_once(
            &options.program,
            &options.child_env,
            &options.next_inputs_dir,
            &input.1,
        );
        fs::rename(
            &input.1,
            options.past_inputs_dir.join(input.1.file_name().unwrap()),
        )
        .expect("Could not move file");
        past_inputs.insert(input.0);
        if past_inputs.len() % 10 == 0 {
            log_info!("Processed {} inputs", past_inputs.len());
        }

        if target(&output) {
            log_info!("Found the target output. Input: {}", input.1.display(),);
            log_debug!(
                "Output:\n{}\nError:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            );
            break;
        }
    }
}

fn execute_once(
    program: &Path,
    env: &HashMap<String, String>,
    next_inputs_dir: &Path,
    input: &Path,
) -> process::Output {
    let input = OpenOptions::new()
        .read(true)
        .open(input)
        .expect("Could not open input file");

    const ENV_PREFIX: &str = "LEAF_";
    use common::config::{CONFIG_STR, CONFIG_STR_FORMAT};
    let config_toml = format!(
        r#"
        [[outputs]]
        type = "file"
        directory = "{dir}"
        format = "binary"
        prefix = "next_"
        extension = ".bin"
        "#,
        dir = next_inputs_dir.display(),
    );
    Command::new(program)
        .env(format!("{ENV_PREFIX}{CONFIG_STR}"), config_toml)
        .env(format!("{ENV_PREFIX}{CONFIG_STR_FORMAT}"), "toml")
        .envs(env)
        .stdin(Stdio::from(input))
        // .stderr(Stdio::inherit())
        .output()
        .expect("Could not execute the program")
}

fn collect_past_inputs(dir: &Path) -> HashSet<String> {
    fs::create_dir_all(dir).expect("Could not create directory");
    list_files(dir)
        .into_iter()
        .map(|(fingerprint, _)| fingerprint)
        .collect()
}

fn grab_new_next_inputs(
    next_dir: &Path,
    current_dir: &Path,
    past_inputs: &HashSet<String>,
    mut current_inputs: HashMap<String, PathBuf>,
) -> HashMap<String, PathBuf> {
    // NOTE: We clean up the next directory for the next execution.
    let count_before = current_inputs.len();
    let mut next_files = list_files(next_dir);
    let next_files = if next_files.is_sorted_by_key(|(_, p)| p) {
        next_files
    } else {
        // Respect the possible ordering in file names.
        next_files.sort_by(|(_, p1), (_, p2)| p1.cmp(p2));
        next_files
    };
    for (fingerprint, path) in next_files {
        log_debug!("Found next input {}", path.display());
        if past_inputs.contains(&fingerprint) || current_inputs.contains_key(&fingerprint) {
            fs::remove_file(&path).expect("Could not remove file");
        } else {
            let new_path = current_dir.join(get_standardized_file_name(
                past_inputs.len() + current_inputs.len(),
                &fingerprint,
            ));
            fs::rename(&path, &new_path).unwrap_or_else(|err| {
                panic!(
                    "Could not move next input {} to {}: {}",
                    path.display(),
                    new_path.display(),
                    err
                )
            });
            current_inputs.insert(fingerprint, new_path);
        }
    }
    if count_before == current_inputs.len() {
        log_debug!("No different next input was found");
    }
    current_inputs
}

fn list_files(dir: &Path) -> Vec<(String, PathBuf)> {
    fs::read_dir(dir)
        .unwrap_or_else(|err| panic!("Could not read directory {}: {}", dir.display(), err))
        .flat_map(|e| e)
        .filter(|e| e.file_type().is_ok_and(|t| t.is_file()))
        .map(|e| e.path())
        .map(|p| (get_fingerprint(&p), p))
        .collect()
}

fn get_fingerprint(file: &Path) -> String {
    let mut sha = Sha256::new();
    let mut file = OpenOptions::new()
        .read(true)
        .open(file)
        .expect("Could not open file");
    std::io::copy(&mut file, &mut sha).expect("Could not read file");
    sha.finalize()
        .iter()
        .map(|b| format!("{:02x}", b))
        .collect()
}

fn get_standardized_file_name(index: usize, fingerprint: &str) -> String {
    format!("input_{:03}_{}.bin", index, &fingerprint[..8])
}
