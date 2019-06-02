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

type PreviewAction =
    | string
    | string[];

// Reducer Managing Dispatched Image Data
const reducer = (state: string[], action: PreviewAction) =>
    typeof action === 'string'
        ? [...state, action]
        : action;


const UploadList = (props: Props & { previews: string[] }) => (
    <div className={styles.UploadList}>
        { props.previews.map(preview => (
            <span>{ preview }</span>
          ))
        }
    </div>
);

const UploadGrid = React.forwardRef(
    ( props: Props & { previews: string[] }
    , ref: React.Ref<HTMLInputElement>
    ) => (
        <React.Fragment>
            { props.previews.map((preview, k) => (
                <div key={k} className={styles.Slot}>
                    <div style={{backgroundImage: `url(${preview})`}} className={styles.Upload}>
                    </div>
                </div>
                ))
            }
        </React.Fragment>
    )
);


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
                                        dispatch(fileOfInterest.name);
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
                                                if (reader.result) {
                                                    dispatch(reader.result.toString());
                                                }
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
