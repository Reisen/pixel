import React, { useState }        from 'react';
import { uploadTags, deleteTags } from '../../api/images';

import Tag                        from '../../components/Tag';
import TextInput                  from '../../components/TextInput';
import styles                     from './TagPanel.module.css';


interface Props {
    editable?: boolean;
    onSave?:   (adds: string[], deletes: string[]) => void;
    tags?:     [string, number][];
    uuid?:     string;
}

const TagPanel = (props: Props) => {
    const [editMode, setEditMode] = useState<boolean>(false);
    const [adds, setAdds]         = useState<string[]>([]);
    const [deletes, setDeletes]   = useState<string[]>([]);
    const [input, setInput]       = useState<string>('');

    const toggleEditMode = () => {
        if (props.uuid && adds.length > 0) {
            uploadTags(props.uuid, { tags: [...adds] });
        }

        if (props.uuid && deletes.length > 0) {
            deleteTags(props.uuid, { tags: [...deletes] });
        }

        // Clear State, Switch Modes.
        props.onSave && props.onSave(adds, deletes);
        setAdds([]);
        setDeletes([]);
        setInput('');
        setEditMode(!editMode)
    }

    const onTagsKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') {
            setAdds([input, ...adds]);
            setInput('');
        }
    };

    return (
        <div>
            <h1>
                Tags

                { props.editable &&
                    <span onClick={toggleEditMode} className={styles.EditButton}>
                        ({ deletes.length > 0 || adds.length > 0
                            ? `Save Changes: +${adds.length}/-${deletes.length}`
                            : editMode ? 'Cancel' : 'Edit'
                        })
                    </span>
                }
            </h1>

            { editMode &&
                <TextInput
                    value={input}
                    onChange={(e) => setInput(e.target.value)}
                    placeholder="Enter Tags Here"
                    onKeyPress={onTagsKeyPress}
                />
            }


            <div className={styles.TagList}>
                { adds.length > 0 &&
                    <div className={styles.TagGroup}>
                        {
                            adds.map((add, k) => (
                                <Tag
                                    new
                                    key={add}
                                    icon="close-circled"
                                    name={add}
                                    value="+"
                                    onIcon={() => {
                                        adds.splice(k, 1);
                                        setAdds([...adds]);
                                    }}
                                />
                            ))
                        }
                    </div>
                }

                { deletes.length > 0 &&
                    <div className={styles.TagGroup}>
                        {
                            deletes.map((del, k) => (
                                <Tag
                                    strikethrough
                                    key={del}
                                    icon="close-circled"
                                    name={del}
                                    value="-"
                                    onIcon={() => {
                                        adds.splice(k, 1);
                                        setDeletes([...adds]);
                                    }}
                                />
                            ))
                        }
                    </div>
                }

                { props.tags &&
                    props.tags.map(tag => (
                        <Tag
                            onIcon={() => setDeletes([tag[0], ...deletes])}
                            key={tag[0]}
                            icon={editMode ? "close-circled" : "tag"}
                            name={tag[0]}
                            strikethrough={deletes.includes(tag[0])}
                            value={tag[1].toString()}
                        />
                    ))
                }
            </div>
        </div>
    )
};

export default TagPanel;
