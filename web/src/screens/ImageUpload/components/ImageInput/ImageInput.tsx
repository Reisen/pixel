// This Component observes a FileInput ref, and upon detecting
// changes automatically renders the contents as a preview. It
// also allows providing a callback to receive the selected data
// for processing.

import React  from 'react';
import styles from './ImageInput.module.css';

interface Props {
    onChange: (imageData: string) => void;
    onClick:  () => void;
}

const ImageInput = React.forwardRef(
    (props: Props, ref: React.Ref<HTMLInputElement>) => {
        // Watch for File Updates, Set Image State to update Background
        const [background, setBackground] = React.useState<string | null>(null);

        // Handles FileInput Changes, updates Image data.
        const onImageChange = (e: React.ChangeEvent<HTMLInputElement>) => {
            const files = e.target.files
                && e.target.files.length > 0
                && e.target.files

            if (files) {
                for (var i = 0; i < files.length; i++) {
                    const reader         = new FileReader();
                    const fileOfInterest = files.item(i);

                    reader.addEventListener('load', () => {
                        if (reader.result) {
                            const data = reader.result.toString();
                            setBackground(data);
                            props.onChange(data);
                        }
                    });

                    if (fileOfInterest) {
                        reader.readAsDataURL(fileOfInterest);
                    }
                }
            }
        };

        return (
            <div
                onClick={props.onClick}
                className={styles.Root}
                style={!background ? {} : {
                    backgroundImage: `url(${background})`
                }}
            >
                <input
                    ref={ref}
                    type="file"
                    accept="image/*"
                    onChange={onImageChange}
                />
            </div>
        );
    }
);

export default ImageInput;
