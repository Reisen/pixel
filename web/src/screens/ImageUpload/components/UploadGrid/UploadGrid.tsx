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

const UploadGrid = React.forwardRef(
    (props: Props, ref: React.Ref<HTMLInputElement>) => {
        const [previews, dispatch] = React.useReducer(
            (state: string[], action: string | string[]) => {
                if (typeof action === 'string') {
                    return [...state, action];
                } else {
                    return action;
                }
            }, []
        );

        console.log('Current Previews: ', previews.length);

        return (
            <div className={styles.Root}>
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
                                console.log('Will Add Previews: ', files && files.length);
                                console.log('Deleting Previews: ', previews.length);
                                dispatch([]);

                                if (files) {
                                    for (var i = 0; i < files.length; i++) {
                                        // Fetch File Information
                                        const fileOfInterest = files.item(i);
                                        if (!fileOfInterest) continue;

                                        // Create a Reader to capture file data
                                        // with for rendering.
                                        const reader = new FileReader();
                                        (function (reader: FileReader) {
                                            reader.addEventListener('load', function() {
                                                if (reader.result) {
                                                    console.log('B) Adding Preview', i);
                                                    const data = reader.result.toString();
                                                    dispatch(data);
                                                    console.log('Previews Updated: ', previews.length);
                                                }
                                            });

                                            reader.readAsDataURL(fileOfInterest);
                                        })(reader);

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

                { previews && previews.map((preview, k) => (
                    <div key={k} className={styles.Slot}>
                        <div style={{backgroundImage: `url(${preview})`}} className={styles.Upload}>
                        </div>
                    </div>
                  ))
                }
            </div>
        );
    }
);

export default UploadGrid;
