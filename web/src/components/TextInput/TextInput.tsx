import React  from 'react';
import styles from './TextInput.module.css';

interface Props {
    icon?:       string;
    placeholder: string;
    value?:      string;
    onChange?:   (e: React.ChangeEvent<HTMLInputElement>) => void;
    onKeyPress?: (e: React.KeyboardEvent<HTMLInputElement>) => void;
}

const TextInput = (props: Props) => (
    <input
        value={props.value}
        onChange={props.onChange}
        onKeyPress={props.onKeyPress}
        className={styles.TextInput}
        placeholder={props.placeholder}
    />
);

export default TextInput;
