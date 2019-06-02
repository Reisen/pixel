// This Component observes a FileInput ref, and upon detecting
// changes automatically renders the contents as a preview. It
// also allows providing a callback to receive the selected data
// for processing.

import React  from 'react';
import styles from './UploadGrid.module.css';

interface Props {
    onChange?: (imageData: string) => void;
    onClick?:  () => void;
}

// Manage Preview Metadata with a React Reducer
// -----------------------------------------------------------------------------

interface Meta {
    name: string;
    data: string;
    size: number;
}

// Reducer Managing Dispatched Image Data
const reducer = (state: Meta[], payload: Meta | Meta[]): Meta[] =>
    Array.isArray(payload)
        ? payload
        : [...state, payload];

// Define Views for both List and Grid
// -----------------------------------------------------------------------------

const UploadList = (props: Props & { previews: Meta[] }) => (
    <div className={styles.UploadList}>
        { props.previews.map(preview => (
            <span key={preview.name}>
                { preview.name }
            </span>
          ))
        }
    </div>
);

const UploadGrid = React.forwardRef(
    ( props: Props & { previews: Meta[] }
    , ref: React.Ref<HTMLInputElement>
    ) => (
        <React.Fragment>
            { props.previews.map((preview, k) => (
                <div key={k} className={styles.Slot}>
                    <div style={{backgroundImage: `url(${preview.data})`}} className={styles.Upload}>
                    </div>
                </div>
              ))
            }
        </React.Fragment>
    )
);

// Wrap Views around an Upliad Input
// -----------------------------------------------------------------------------

const UploadWrapper = React.forwardRef(
    (props: Props, ref: React.Ref<HTMLInputElement>) => {
        // Track Renderable Previews, the Limit is used to decide whether to
        // render a grid view or a list view.
        const previewCountLimit    = 5;
        const [previews, dispatch] = React.useReducer(reducer, []);

        const UploadSlot = () => (
            <div className={styles.Slot}>
                <div className={styles.UploadSquare}>
                    <input
                        multiple
                        ref={ref}
                        type="file"
                        accept="image/*"
                        onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                            const files = e.target.files
                                && e.target.files.length > 0
                                && e.target.files

                            // Clear out old previews.
                            dispatch([]);

                            if (files) {
                                // Push Filenames
                                if (files.length > previewCountLimit) {
                                    for (let i = 0; i < files.length; i++) {
                                        // Fetch File Information
                                        const fileOfInterest = files.item(i);
                                        if (!fileOfInterest) continue;
                                        dispatch({
                                            name: fileOfInterest.name,
                                            size: fileOfInterest.size,
                                            data: '',
                                        });
                                    }
                                } else {
                                    for (let i = 0; i < files.length; i++) {
                                        // Fetch File Information
                                        const fileOfInterest = files.item(i);
                                        if (!fileOfInterest) continue;

                                        // Create a Reader to capture file data
                                        // with for rendering.
                                        const reader = new FileReader();
                                        (function (reader: FileReader) {
                                            reader.addEventListener('load', () => {
                                                reader.result && dispatch({
                                                    data: reader.result.toString(),
                                                    name: '',
                                                    size: 0
                                                });
                                            });

                                            reader.readAsDataURL(fileOfInterest);
                                        })(reader);
                                    }
                                }
                            }
                        }}
                    />
                    <span>
                        <i className="icofont-upload"/><br/>
                        Choose Files
                    </span>
                </div>
            </div>
        );

        const UploadStat = () => (
            <div className={styles.Stats}>
                Stats! { previews.length }
            </div>
        );

        return (
            <div className={styles.Root}>
                { previews.length < previewCountLimit ? <UploadSlot/> : <UploadStat/> }
                { previews.length < previewCountLimit
                    ? <UploadGrid previews={previews}/>
                    : <UploadList previews={previews}/>
                }
            </div>
        );
    }
);

export default UploadWrapper;
